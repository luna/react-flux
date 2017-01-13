{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables, DeriveAnyClass,
             FlexibleInstances, DeriveGeneric, BangPatterns, TemplateHaskell #-}
module Main (main) where

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Maybe
import Debug.Trace
import GHC.Generics (Generic)
import Data.Time (UTCTime(..), fromGregorian)
import React.Flux
import React.Flux.Lifecycle
import React.Flux.Addons.React
import React.Flux.Addons.Intl
import qualified Data.Text as T

import GHCJS.Types (JSVal, JSString)
import GHCJS.Marshal (fromJSVal)
import JavaScript.Array (JSArray)
import qualified Data.JSString.Text as JSS

foreign import javascript unsafe
    "(function(x) { \
    \    if (!window.test_client_output) window.test_client_output = []; \
    \    window.test_client_output.push(x); \
    \})($1)"
    js_output :: JSString -> IO ()

data OutputStoreData = OutputStoreData
    deriving (Show, Typeable)

instance StoreData OutputStoreData where
    type StoreAction OutputStoreData = [T.Text]
    -- log both to the console and to js_output
    transform ss OutputStoreData = do
        mapM_ (js_output . JSS.textToJSString) ss
        trace (unlines $ map T.unpack ss) $ return OutputStoreData

outputStore :: ReactStore OutputStoreData
outputStore = unsafeMkStore OutputStoreData

output :: [T.Text] -> [SomeStoreAction]
output s = [SomeStoreAction outputStore s]

outputIO :: [T.Text] -> IO ()
outputIO ss = void $ transform ss OutputStoreData

foreign import javascript unsafe
  "React['createElement']('p', {'id': 'test-raw-js-para'}, $2)"
  js_testRawJs :: JSVal -> JSArray -> IO JSVal

--------------------------------------------------------------------------------
--- Events
--------------------------------------------------------------------------------

logM :: (T.Text -> Bool) -> T.Text
logM f = "alt modifier: " <> (T.pack $ show (f "Alt"))

logT :: EventTarget -> T.Text
logT t = eventTargetProp t "id"

tshow :: Show a => a -> T.Text
tshow = T.pack . show

rawShowView :: ReactView Int
rawShowView = defineView "raw show view" elemShow

eventsView :: ReactView ()
eventsView = defineView "events" $ \() ->
    div_ $ do
        p_ $ input_ [ "type" $= "text"
                    , "id" $= "keyinput"
                    , "placeholder" $= "onKeyDown"
                    , onKeyDown $ \e k -> output
                        [ "keydown"
                        , tshow e
                        , tshow k
                        , logM (keyGetModifierState k)
                        , logT (evtTarget e)
                        , logT (evtCurrentTarget e)
                        ]
                    , onFocus $ \e _ -> output
                        [ "focus"
                        , tshow e
                        --, logT $ focusRelatedTarget f
                        ]
                    ]

        p_ $ label_ [ "id" $= "clickinput"
                    , onClick $ \e m -> output
                        [ "click"
                        , tshow e
                        , tshow m
                        , logM (mouseGetModifierState m)
                        --, logT (mouseRelatedTarget m)
                        ]
                    ]
           "onClick"

        p_ $ label_ [ "id" $= "touchinput"
                    , onTouchStart $ \e t -> output
                        [ "touchstart"
                        , tshow e
                        , tshow t
                        , logM (touchGetModifierState t)
                        , logT (touchTarget $ head $ touchTargets t)
                        , "endtouch"
                        ]
                    ]
           "onTouchStart"

        p_ $ a_ [ "id" $= "some-link"
                , "href" $= "http://www.haskell.org"
                , onClick $ \e _ -> output ["Click some-link"] ++ [preventDefault e]
                ]
                "Testing preventDefault"

        div_ $
            div_ [ "id" $= "outer-div"
                 , onClick $ \_ _ -> output ["Click on outer div"]
                 , capturePhase $ onDoubleClick $ \e _ -> output ["Double click outer div"] ++ [stopPropagation e]
                 ] $ do

                span_ [ "id" $= "inner-span"
                      , onClick $ \e _ -> output ["Click inner span"] ++ [stopPropagation e]
                      , onDoubleClick $ \_ _ -> output ["Double click inner span"]
                      ]
                      "Testing stopPropagation"

        p_ [ "id" $= "raw-show-view"] $ view rawShowView 42 mempty

eventsView_ :: ReactElementM eventHandler ()
eventsView_ = view eventsView () mempty

--------------------------------------------------------------------------------
--- Lifecycle
--------------------------------------------------------------------------------

logPandS :: LPropsAndState T.Text Int -> IO ()
logPandS ps = do
    p <- lGetProps ps
    st <- lGetState ps
    outputIO ["Current props and state: " <> p <> ", " <> (T.pack $ show st)]

foreign import javascript unsafe
    "$1.id"
    js_domGetId :: JSVal -> IO JSVal

logDOM :: LDOM -> IO ()
logDOM dom = do
    this <- lThis dom >>= js_domGetId >>= fromJSVal
    x <- lRef dom "refSt" >>= js_domGetId >>= fromJSVal
    y <- lRef dom "refProps" >>= js_domGetId >>= fromJSVal
    outputIO [ "this id = " <> fromMaybe "Nothing" this
             , "refStr id = " <> fromMaybe "Nothing" x
             , "refProps id = " <> fromMaybe "Nothing" y
             ]


testLifecycle :: ReactView T.Text
testLifecycle = defineLifecycleView "testlifecycle" (12 :: Int) lifecycleConfig
    { lRender = \s p -> p_ ["id" $= "lifecycle-p"] $ do
        span_ "Current state: "
        span_ ["ref" $= "refSt", "id" $= "hello"] (elemShow s)
        span_ ["ref" $= "refProps", "id" $= "world"] $ elemText $ "Current props: " <> p
        button_ [ "id" $= "increment-state"
                , onClick $ \_ _ st -> ([], Just $ st + 1)
                ] "Incr"

    , lComponentWillMount = Just $ \pAndS setStateFn -> do
        outputIO ["will mount"]
        logPandS pAndS
        setStateFn 100

    , lComponentDidMount = Just $ \pAndS dom _setStateFn -> do
        outputIO ["did mount"]
        logPandS pAndS
        logDOM dom

    , lComponentWillReceiveProps = Just $ \pAndS _dom _setStateFn newProps -> do
        outputIO ["will recv props"]
        logPandS pAndS
        outputIO ["New props: " <> newProps]

    , lComponentWillUpdate = Just $ \pAndS _dom newProps newState -> do
        outputIO ["will update"]
        logPandS pAndS
        outputIO ["New props: " <> newProps, "New state: " <> tshow newState]

    , lComponentDidUpdate = Just $ \pAndS _dom _setStateFn oldProps oldState -> do
        outputIO ["did update"]
        logPandS pAndS
        outputIO ["Old props: " <> oldProps, "Old state: " <> tshow oldState]

    , lComponentWillUnmount = Just $ \pAndS _dom -> do
        outputIO ["will unmount"]
        logPandS pAndS
    }

testLifecycle_ :: T.Text -> ReactElementM eventHandler ()
testLifecycle_ s = view testLifecycle s $ span_ ["id" $= "child-passed-to-view"] "I am a child!!!"

--------------------------------------------------------------------------------
--- Children passed to view
--------------------------------------------------------------------------------

displayChildren :: ReactView String
displayChildren = defineView "display children" $ \ident ->
    span_ [classNames [("display-children", True), ("missing-name", False)], "id" @= ident]
        childrenPassedToView

displayChildren_ :: String -> ReactElementM handler () -> ReactElementM handler ()
displayChildren_ = view displayChildren

displayChildrenSpec :: ReactElementM handler ()
displayChildrenSpec = ul_ $ do
    li_ $ displayChildren_ "empty-children" mempty
    li_ $ displayChildren_ "single-child-wrapper" $ span_ ["id" $= "single-child"] "Single Child!!"
    li_ $ displayChildren_ "multi-child" $ span_ ["id" $= "child1"] "Child 1" <> span_ ["id" $= "child2"] "Child 2"

--------------------------------------------------------------------------------
--- CSS Transitions
--------------------------------------------------------------------------------

cssTransitions :: ReactView [T.Text]
cssTransitions = defineView "css transitions" $ \items ->
    div_ ["id" $= "css-transitions"] $
        cssTransitionGroup ["transitionName" $= "example"] $
            forM_ (zip items [(0 :: Int)..]) $ \(txt, key) ->
                div_ ["key" @= key] $ span_ ["className" $= "css-transition-entry"] $ elemText txt

--------------------------------------------------------------------------------
--- shouldComponentUpdate
--------------------------------------------------------------------------------

data ShouldComponentUpdateData = ShouldComponentUpdateData Int String
    deriving (Eq, Typeable, Show)

-- | The data in the store is four 'ShouldComponentUpdateData's.  The reason is we test
-- views with tuples of size up to 3, and so we want 4 entries in the store to be able to
-- test editing the store but not changing anything that is passed to a view, to test that
-- the shouldComponentUpdate function is working properly.
data ShouldComponentUpdate = ShouldComponentUpdate {
    scu1 :: !ShouldComponentUpdateData -- ^ passed to all three views
  , scu2 :: !ShouldComponentUpdateData -- ^ only passed to the pair view
  , scu3 :: !ShouldComponentUpdateData -- ^ only passed to the triple view
  , scu4 :: !ShouldComponentUpdateData -- ^ not passed to any view
} deriving (Eq, Typeable, Show)

data SCUIndex = SCU1 | SCU2 | SCU3 | SCU4
    deriving (Show, Eq, Typeable, Generic, NFData, Bounded, Enum)

data ShouldComponentUpdateAction = IncrementAllSCUData SCUIndex
                                 | IncrementFirstSCUData SCUIndex
                                 | NoChangeToSCUData
    deriving (Show, Typeable, Generic, NFData)

toggleSCU :: ShouldComponentUpdateData -> ShouldComponentUpdateData
toggleSCU (ShouldComponentUpdateData i s) = ShouldComponentUpdateData (i+1) s

instance StoreData [ShouldComponentUpdate] where
    type StoreAction [ShouldComponentUpdate] = ShouldComponentUpdateAction
    transform _ [] = error "Will never happen"
    transform action ds@(first:rest) =
        pure $ case action of
            NoChangeToSCUData -> ds
            (IncrementAllSCUData SCU1) -> [ShouldComponentUpdate (toggleSCU s1) s2 s3 s4 | ShouldComponentUpdate s1 s2 s3 s4 <- ds]
            (IncrementAllSCUData SCU2) -> [ShouldComponentUpdate s1 (toggleSCU s2) s3 s4  | ShouldComponentUpdate s1 s2 s3 s4 <- ds]
            (IncrementAllSCUData SCU3) -> [ShouldComponentUpdate s1 s2 (toggleSCU s3) s4 | ShouldComponentUpdate s1 s2 s3 s4 <- ds]
            (IncrementAllSCUData SCU4) -> [ShouldComponentUpdate s1 s2 s3 (toggleSCU s4) | ShouldComponentUpdate s1 s2 s3 s4 <- ds]
            (IncrementFirstSCUData SCU1) -> first { scu1 = toggleSCU $ scu1 first } : rest
            (IncrementFirstSCUData SCU2) -> first { scu2 = toggleSCU $ scu2 first } : rest
            (IncrementFirstSCUData SCU3) -> first { scu3 = toggleSCU $ scu3 first } : rest
            (IncrementFirstSCUData SCU4) -> first { scu4 = toggleSCU $ scu4 first } : rest

shouldComponentUpdateStore :: ReactStore [ShouldComponentUpdate]
shouldComponentUpdateStore = unsafeMkStore
        [ ShouldComponentUpdate (mkS 1 "Quick Ben") (mkS 2 "Whiskeyjack") (mkS 3 "Fiddler") (mkS 4 "Kellanved")
        , ShouldComponentUpdate (mkS 5 "Karsa") (mkS 6 "Tehol") (mkS 7 "Tayschrenn") (mkS 8 "Kruppe")
        , ShouldComponentUpdate (mkS 9 "Anomander Rake") (mkS 10 "Iskaral Pust") (mkS 11 "Dujek") (mkS 12 "Tavore")
        ]
    where
        mkS = ShouldComponentUpdateData

-- | This will log wheenver componentWillUpdate lifecycle event occurs
logComponentWillUpdate :: ReactView ShouldComponentUpdateData
logComponentWillUpdate = defineLifecycleView "shouldComponentUpdate single spec" () lifecycleConfig
    { lRender = \() (ShouldComponentUpdateData i s) ->
                    span_ (elemShow i) <> span_ (elemString s)
    , lComponentWillUpdate = Just $ \curProps _ (ShouldComponentUpdateData newI newS) () -> do
          ShouldComponentUpdateData curI curS <- lGetProps curProps
          outputIO [ "Component will update single"
                   , "current props: " <> tshow curI <> " " <> T.pack curS
                   , "new props: " <> tshow newI <> " " <> T.pack newS
                   ]
    }

logComp1_ :: ShouldComponentUpdateData -> Int -> ReactElementM handler ()
logComp1_ !sc i = viewWithKey logComponentWillUpdate i sc mempty

logComponentWillUpdatePair :: ReactView (ShouldComponentUpdateData, ShouldComponentUpdateData)
logComponentWillUpdatePair = defineLifecycleView "shouldComponentUpdate pair spec" () lifecycleConfig
    { lRender = \() (ShouldComponentUpdateData i1 s1, ShouldComponentUpdateData i2 s2) ->
                    span_ (elemShow i1) <> span_ (elemString s1) <> span_ (elemShow i2) <> span_ (elemString s2)
    , lComponentWillUpdate = Just $ \curProps _ (ShouldComponentUpdateData newI1 newS1, ShouldComponentUpdateData newI2 newS2) () -> do
          (ShouldComponentUpdateData curI1 curS1, ShouldComponentUpdateData curI2 curS2) <- lGetProps curProps
          outputIO [ "Component will update for pair input view"
                   , T.pack $ "current props: " ++ show curI1 ++ " " ++ curS1 ++ " " ++ show curI2 ++ " " ++ curS2
                   , T.pack $ "new props: " ++ show newI1 ++ " " ++ newS1 ++ " " ++ show newI2 ++ " " ++ newS2
                   ]
    }

logComp2_ :: ShouldComponentUpdateData -> ShouldComponentUpdateData -> Int -> ReactElementM handler ()
logComp2_ !sc1 !sc2 i = viewWithKey logComponentWillUpdatePair i (sc1, sc2) mempty

logComponentWillUpdateTriple :: ReactView (ShouldComponentUpdateData, ShouldComponentUpdateData, ShouldComponentUpdateData)
logComponentWillUpdateTriple = defineLifecycleView "shouldComponentUpdate triple spec" () lifecycleConfig
    { lRender = \() (ShouldComponentUpdateData i1 s1, ShouldComponentUpdateData i2 s2, ShouldComponentUpdateData i3 s3) ->
                  span_ (elemShow i1) <> span_ (elemString s1) <> span_ (elemShow i2) <> span_ (elemString s2) <> span_ (elemShow i3) <> span_ (elemString s3)
    , lComponentWillUpdate = Just $ \curProps _
        (ShouldComponentUpdateData newI1 newS1, ShouldComponentUpdateData newI2 newS2, ShouldComponentUpdateData newI3 newS3) () -> do
            (ShouldComponentUpdateData curI1 curS1, ShouldComponentUpdateData curI2 curS2, ShouldComponentUpdateData curI3 curS3) <- lGetProps curProps
            outputIO [ "Component will update for triple input view"
                     , T.pack $ "current props: " ++ show curI1 ++ " " ++ curS1 ++ " " ++ show curI2 ++ " " ++ curS2 ++ " " ++ show curI3 ++ " " ++ curS3
                     , T.pack $ "new props: " ++ show newI1 ++ " " ++ newS1 ++ " " ++ show newI2 ++ " " ++ newS2 ++ " " ++ show newI3 ++ " " ++ newS3
                     ]
    }

logComp3_ :: ShouldComponentUpdateData -> ShouldComponentUpdateData -> ShouldComponentUpdateData -> Int -> ReactElementM handler ()
logComp3_ !sc1 !sc2 !sc3 i = viewWithKey logComponentWillUpdateTriple i (sc1, sc2, sc3) mempty

shouldComponentUpdateSpec :: ReactView ()
shouldComponentUpdateSpec = defineControllerView "should component update" shouldComponentUpdateStore $ \ds () ->
    div_ ["id" $= "should-component-update"] $ do
        ul_ ["id" $= "should-component-update-single"] $ forM_ (zip ds [(0 :: Int)..]) $ \(d,i)  ->
            li_ $ logComp1_ (scu1 d) i
        ul_ ["id" $= "should-component-update-pair"]  $ forM_ (zip ds [(0 :: Int)..]) $ \(d,i)  ->
            li_ $ logComp2_ (scu1 d) (scu2 d) i
        ul_ ["id" $= "should-component-update-triple"]  $ forM_ (zip ds [(0 :: Int)..]) $ \(d, i)  ->
            li_ $ logComp3_ (scu1 d) (scu2 d) (scu3 d) i

        button_ ["id" $= "no-change-scu", onClick $ \_ _ -> [SomeStoreAction shouldComponentUpdateStore NoChangeToSCUData]]
            "No change to data"

        forM_ [minBound..maxBound] $ \idx -> do
            button_ ["id" @= ("change-all-scu-" ++ show idx), onClick $ \_ _ -> [SomeStoreAction shouldComponentUpdateStore $ IncrementAllSCUData idx]] $
                elemString $ "Increment all " ++ show idx
            button_ ["id" @=("increment-first-scu" ++ show idx), onClick $ \_ _ -> [SomeStoreAction shouldComponentUpdateStore $ IncrementFirstSCUData idx]] $
                elemString $ "Increment first entry's integer" ++ show idx

--------------------------------------------------------------------------------
--- Callback returning view
--------------------------------------------------------------------------------

data CallbackViewProps = CallbackViewProps Int String
    deriving (Eq, Show, Typeable)

callbackArgsToProps :: Int -> String -> ReturnProps CallbackViewProps
callbackArgsToProps i s = ReturnProps $ CallbackViewProps i s

callbackViewTest :: ReactView CallbackViewProps
callbackViewTest = defineView "callback view props test" $ \(CallbackViewProps i s) ->
    p_ [ "id" $= "callback-view-props-test"] $
        elemString $ "Props are " ++ show i ++ " and " ++ s

foreign import javascript unsafe
    "React['createClass']({'displayName':'callback wrapper', 'render': function() { \
    \ return React['createElement']('div', {}, [React.createElement('p', {}, 'From Callback'), this.props.foo(5, 'Hello World')]); \
    \ }})"
    js_createWrapperClass :: JSVal

callbackViewWrapper :: ReactView ()
callbackViewWrapper = defineView "callback view wrapper" $ \() ->
    div_ ["id" $= "callback-view-wrapper"] $
        foreignClass js_createWrapperClass [ callbackViewWithProps "foo" callbackViewTest callbackArgsToProps ] mempty

--------------------------------------------------------------------------------
--- Intl
--------------------------------------------------------------------------------

foreign import javascript unsafe
    "{'with_trans': 'message from translation {abc}'}"
    js_translations :: JSVal

intlSpec :: ReactView ()
intlSpec = defineView "intl" $ \() ->
    intlProvider_ "en" (Just js_translations) Nothing $
        view intlSpecBody () mempty

intlSpecBody :: ReactView ()
intlSpecBody = defineView "intl body" $ \() -> div_ ["id" $= "intl-spec"] $
    ul_ $ do
        li_ ["id" $= "f-number"] $
            formattedNumber_ [ "value" @= (0.9 :: Double), "style" $= "percent" ]
        li_ ["id" $= "f-int"] $ int_ 100000
        li_ ["id" $= "f-double"] $ double_ 40000.2
        li_ ["id" $= "f-number-prop"] $
            input_ [formattedNumberProp "placeholder" (123456 :: Int) []]

        let moon = fromGregorian 1969 7 20
            fullDayF = DayFormat { weekdayF = Just "long", eraF = Just "short", yearF = Just "2-digit", monthF = Just "long", dayF = Just "2-digit" }

        li_ ["id" $= "f-shortday"] $ day_ shortDate moon
        li_ ["id" $= "f-fullday"] $ day_ fullDayF moon
        li_ ["id" $= "f-date"] $ formattedDate_ (Left moon)
                [ "weekday" $= "short", "month" $= "short", "day" $= "numeric", "year" $= "2-digit" ]
        li_ ["id" $= "f-date-prop"] $
            input_ [formattedDateProp "placeholder" (Left moon) []]

        let step = UTCTime moon (2*60*60 + 56*60) -- 1969-7-20 02:56 UTC
            fullT = (fullDayF, TimeFormat { hourF = Just "numeric", minuteF = Just "2-digit", secondF = Just "numeric", timeZoneNameF = Just "long" })

        li_ ["id" $= "f-shorttime"] $ utcTime_ shortDateTime step
        li_ ["id" $= "f-fulltime"] $ utcTime_ fullT step
        li_ ["id" $= "f-time"] $ formattedDate_ (Right step)
                [ "year" $= "2-digit", "month" $= "short", "day" $= "numeric"
                , "hour" $= "numeric", "minute" $= "2-digit", "second" $= "numeric"
                , "timeZoneName" $= "short"
                , "timeZone" $= "Pacific/Tahiti"
                ]
        li_ ["id" $= "f-time-prop"] $
            input_ [formattedDateProp "placeholder" (Right step)
                    [ "year" `iprop` ("2-digit" :: String)
                    , "month" `iprop` ("short" :: String)
                    , "day" `iprop` ("2-digit" :: String)
                    , "hour" `iprop` ("numeric" :: String)
                    , "timeZone" `iprop` ("Pacific/Tahiti" :: String)
                    ]
                   ]

        li_ ["id" $= "f-relative"] $ relativeTo_ step
        li_ ["id" $= "f-relative-days"] $ formattedRelative_ step [ "units" $= "day" ]

        li_ ["id" $= "f-plural"] $ plural_ [ "value" @= (100 :: Int), "one" $= "plural one", "other" $= "plural other"]
        li_ ["id" $= "f-plural-prop"] $
            input_ [pluralProp "placeholder" (100 :: Int) ["one" `iprop` ("plural one" :: String), "other" `iprop` ("plural other" :: String)]]

        li_ ["id" $= "f-msg"] $
            $(message "photos" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}} {takenAgo}.")
                [ "name" $= "Neil Armstrong"
                , "numPhotos" @= (100 :: Int)
                , elementProperty "takenAgo" $ span_ ["id" $= "takenAgoSpan"] "years ago"
                ]

        li_ ["id" $= "f-msg-prop"] $
            input_ [ $(messageProp "placeholder" "photosprop" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}}")
                [ "name" `iprop` ("Neil Armstrong" :: String)
                , "numPhotos" `iprop` (100 :: Int)
                ]
            ]

        li_ ["id" $= "f-msg-with-trans"] $
            $(message "with_trans" "this is not used {abc}") ["abc" $= "xxx"]

        li_ ["id" $= "f-msg-with-descr"] $
            $(message' "photos2" "How many photos?" "{name} took {numPhotos, plural, =0 {no photos} =1 {one photo} other {# photos}}.")
                [ "name" $= "Neil Armstrong"
                , "numPhotos" @= (0 :: Int)
                ]

        li_ ["id" $= "f-msg-prop-with-descr"] $
            input_ [$(messageProp' "placeholder" "photosprop2" "How many photos?" "{name} took {numPhotos, number} photos")
                        [ "name" `iprop` ("Neil Armstrong" :: String)
                        , "numPhotos" `iprop` (0 :: Int)
                        ]
                   ]

        li_ ["id" $= "f-html-msg"] $
            $(htmlMsg "html1" "<b>{num}</b> is the answer to life, the universe, and everything")
                [ "num" @= (42 :: Int) ]

        li_ ["id" $= "f-html-msg-with-descr"] $
            $(htmlMsg' "html2" "Hitchhiker's Guide" "{num} is the <b>answer</b> to life, the universe, and everything")
                [ "num" @= (42 :: Int) ]

--------------------------------------------------------------------------------
--- Main
--------------------------------------------------------------------------------

-- | Test a lifecycle view with all lifecycle methods nothing
testClient :: ReactView ()
testClient = defineLifecycleView "app" "Hello" lifecycleConfig
    { lRender = \s () -> do
        eventsView_

        when (s /= "") $
            testLifecycle_ s
        button_ [ "id" $= "add-app-str"
                , onClick $ \_ _ s' -> ([], Just $ s' <> "o")
                ]
                "Add o"
        button_ [ "id" $= "clear-app-str"
                , onClick $ \_ _ _ -> ([], Just "")
                ] "Clear"

        displayChildrenSpec

        view cssTransitions ["A", "B"] mempty

        view shouldComponentUpdateSpec () mempty

        view callbackViewWrapper () mempty

        view intlSpec () mempty

        rawJsRendering js_testRawJs $
          span_ ["id" $= "test-raw-js-body"]
            "Raw Javascript Render Body"
    }

main :: IO ()
main = reactRender "app" testClient ()

writeIntlMessages (intlFormatJson "test/client/msgs/jsonmsgs.json")
writeIntlMessages (intlFormatJsonWithoutDescription "test/client/msgs/jsonnodescr.json")
writeIntlMessages (intlFormatAndroidXML "test/client/msgs/android.xml")
