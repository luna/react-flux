{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}


-- |

module EventTest where

import           React.Flux                   hiding (view)
import qualified React.Flux                   as RF

import           Control.DeepSeq
import qualified Data.Text                    as T
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)

#ifdef __GHCJS__
import Data.JSString (JSString)
#endif

data ETState = ETState
    { etActions :: [ETAction]
    , etText    :: !T.Text
    , etBool    :: !Bool
    } deriving (Eq, Show, Typeable)

data ETAction
    = ETInit
    | ClearActionsA
    | KeyDownEnterA
    | KeyUpEnterA
    | KeyPressCapitalA
    | FocusA
    | BlurA
    | TextChangeA T.Text
    | BoolChangeA Bool
    | MouseEnterA Int
    | MouseLeaveA Int
    | MouseOverA Int
    | MouseOutA Int
    | MouseDownA
    | MouseUpA
    | ClickA
    | DoubleClickA
    | MouseMoveA Int Int
    | ScrollA Int
    deriving (Eq, Show, Typeable, Generic, NFData)

instance StoreData ETState where
  type StoreAction ETState = ETAction
  transform action st = do
    let st1 = st{etActions = action:etActions st}
        st2 = case action of
          ETInit           -> st1
          ClearActionsA    -> st1{etActions = []}
          TextChangeA txt  -> st1{etText = txt}
          BoolChangeA bool -> st1{etBool = bool}
          _                -> st1
    return st2

store :: ReactStore ETState
store = mkStore $ ETState [] "" True

dispatchEventTest :: ETAction -> [SomeStoreAction]
dispatchEventTest a = [SomeStoreAction store a]

view :: ReactView ETState
view = defineView "event-tests" $ \(ETState actions text bool) ->
  section_ $ do
    let textVal = "value" &= text

    div_ ["className" $= "et-actions"] $ do
      a_ [onClick $ \_ _ -> dispatchEventTest ClearActionsA] $
        elemText "Clear event history"
      pre_ $
        elemString $ unlines $ map show actions

    h2_ $ elemText "Testing keyboard events"
    p_ $ elemText "Type <enter> to test onKeyUp/onKeyDown"
    input_ ["type" $= "text"
            , onKeyDown $ \_ ke ->
            case keyCode ke of
            13 -> dispatchEventTest KeyDownEnterA
            _ -> []
            , onKeyUp $ \_ ke ->
            case keyCode ke of
            13 -> dispatchEventTest KeyUpEnterA
            _ -> []]
    p_ $ elemText "Type a capital letter to test onKeyPress"
    input_ ["type" $= "text"
           , onKeyPress $ \_ ke ->
           let chars = map fromEnum ['A'..'Z']
           in
             if keyEvtCharCode ke `elem` chars
             then dispatchEventTest KeyPressCapitalA
             else []
           ]
    h2_ $ elemText "Testing focus events"
    p_ $ elemText "Use the box below to test onBlur/onFocus"
    input_ ["type" $= "text"
           , onFocus $ \_ _ -> dispatchEventTest FocusA
           , onBlur $ \_ _ -> dispatchEventTest BlurA
           ]

    h2_ $ elemText "Testing form events"
    p_ $ elemText "These elements share a text value. Use them to test onChange."
    p_ $ input_ [textVal,
                 onChange $ \evt ->
                 dispatchEventTest (TextChangeA $ targetText evt "value")
                ]
    p_ $ textarea_ [textVal
                   , onChange $ \evt ->
                   dispatchEventTest (TextChangeA $ targetText evt "value")
                   ] mempty
    p_ $ select_ [textVal, onChange $ \evt ->
                    dispatchEventTest (TextChangeA $ targetText evt "value")] $ do
      option_ ["value" $= "sheep"] $ elemText "Sheep"
      option_ ["value" $= "pig"  ] $ elemText "Pig"
      option_ ["value" $= "cow"  ] $ elemText "Cow"
    p_ $ elemText "These elements share a boolean value. Use them to test onChange."
    p_ $
      input_ ["type" $= "checkbox"
             , "checked" @= bool
             , onChange $ \evt ->
             dispatchEventTest (BoolChangeA $ targetBool evt "checked")
             ]
    p_ $ do
      input_ ["type" $= "radio"
             , "checked" @= bool
             , onChange $ \evt ->
             dispatchEventTest (BoolChangeA $ targetBool evt "checked")
             ]
      input_ ["type" $= "radio"
             , "checked" @= not bool
             , onChange $ \evt ->
             dispatchEventTest (BoolChangeA $ not $ targetBool evt "checked")
             ]

    h2_ $ elemText "Testing mouse events"
    p_ $ elemText "Use the box below to test onMouseDown, onMouseUp, onClick, and onDoubleClick."
    div_  ["className" $= "et-box4"
          , onMouseDown   $ \_ _ -> dispatchEventTest MouseDownA
          , onMouseUp     $ \_ _ -> dispatchEventTest MouseUpA
          , onClick       $ \_ _ -> dispatchEventTest ClickA
          , onDoubleClick $ \_ _ -> dispatchEventTest DoubleClickA
          ] $
      span_ $ elemText "Click me!"
    p_ $ elemText "Use the boxes below to test onMouseEnter, onMouseLeave, onMouseOver, and onMouseOut."
    div_ ["className" $= "et-box1"
         , onMouseOver  $ \_ _ -> dispatchEventTest $ MouseOverA 1
         , onMouseOut   $ \_ _ -> dispatchEventTest $ MouseOutA 1
         , onMouseEnter $ \_ _ -> dispatchEventTest $ MouseEnterA 1
         , onMouseLeave $ \_ _ -> dispatchEventTest $ MouseLeaveA 1] $ do
      span_ $ elemText "1"
      div_ ["className" $= "et-box2"
           , onMouseOver  $ \_ _ -> dispatchEventTest $ MouseOverA 2
           , onMouseOut   $ \_ _ -> dispatchEventTest $ MouseOutA 2
           , onMouseEnter $ \_ _ -> dispatchEventTest $ MouseEnterA 2
           , onMouseLeave $ \_ _ -> dispatchEventTest $ MouseLeaveA 2] $ do
        span_ $ elemText "2"
        div_ ["className" $= "et-box3"
             , onMouseOver  $ \_ _ -> dispatchEventTest $ MouseOverA 2
             , onMouseOut   $ \_ _ -> dispatchEventTest $ MouseOutA 2
             , onMouseEnter $ \_ _ -> dispatchEventTest $ MouseEnterA 3
             , onMouseLeave $ \_ _ -> dispatchEventTest $ MouseLeaveA 3] $
          elemText "3"
    p_ $ elemText "Use the box below to test onMouseMove"
    div_ ["className" $= "et-box4"
         , onMouseMove $ \_ me ->
         dispatchEventTest $ MouseMoveA  (mousePageX me) (mousePageY me)
         ] $
      span_ $ elemText "Move mouse here"

    h2_ $ elemText "Testing scroll events"
    p_ $ elemText "Use the div below to test onScroll."
    div_ ["className" $= "et-box5"
          , onScroll  $ \evt ->
          dispatchEventTest $ ScrollA (targetInt evt "scrollTop")
          ] $
      elemText "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras at ligula orci. Nam efficitur ante justo, eget ultrices mauris maximus ut. Maecenas laoreet a lorem non fermentum. Vestibulum maximus porta quam, a pellentesque elit tristique eget. Sed sit amet lacus ut nunc malesuada aliquam. Sed leo erat, aliquam sagittis massa non, tempor condimentum ipsum. Nullam efficitur vel metus ac pharetra. Aenean in feugiat urna. Aenean in ante fermentum, sagittis ligula vel, malesuada arcu. Aliquam accumsan varius tempor. Etiam consectetur augue lorem, sit amet ullamcorper lacus finibus ac. Etiam semper euismod suscipit. Sed dapibus consectetur lacus, ac feugiat purus. Etiam lobortis ac leo et vehicula. Nullam pellentesque, lectus sed imperdiet viverra, odio felis rutrum sem, nec tristique ex enim eget velit. Phasellus semper venenatis metus, in sagittis mauris convallis et. Donec luctus ex a blandit sollicitudin. Vestibulum sollicitudin ipsum ut arcu ultrices molestie. Nulla id lorem tincidunt, ullamcorper libero ac, fringilla felis. Aenean rutrum et nisi eget commodo. Phasellus scelerisque ante eleifend elementum facilisis. Proin enim nisi, dapibus id nunc ac, dictum tincidunt neque. Cras ac viverra dolor, eu tincidunt nisl. Pellentesque finibus, massa pellentesque laoreet auctor, nisl odio pharetra velit, vel sollicitudin mi eros ac lectus."

view_ :: ETState -> ReactElementM eventHandler ()
view_ st =
  RF.view view st mempty

#ifdef __GHCJS__

targetInt :: Event -> JSString -> Int
targetInt = target

targetText :: Event -> JSString -> T.Text
targetText = target

targetBool :: Event -> JSString -> Bool
targetBool = target

#else

targetInt :: Event -> String -> Int
targetInt _ _ = 1

targetText :: Event -> String -> T.Text
targetText _ _ = ""

targetBool :: Event -> String -> Bool
targetBool _ _ = True

#endif
