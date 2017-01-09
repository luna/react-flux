-- | Internal module containing the store definitions.
module React.Flux.Store (
    ReactStoreRef(..)
  , ReactStore(..)
  , StoreData(..)
  , SomeStoreAction(..)
  , mkStore
  , unsafeMkStore
  , getStoreData
  , alterStore
  , modifyStore
  , modifyStoreIf
  , executeAction
) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.DeepSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

#ifdef __GHCJS__
import GHCJS.Types (JSVal, isNull, IsJSVal)
import React.Flux.Export (Export, export)
#else
type JSVal = ()
class IsJSVal a
#endif

-- | This type is used to represent the foreign javascript object part of the store.
newtype ReactStoreRef storeData = ReactStoreRef JSVal deriving (Generic)
instance IsJSVal (ReactStoreRef storeData)

-- | A store contains application state, receives actions from the dispatcher, and notifies
-- controller-views to re-render themselves.  You can have multiple stores; it should be the case
-- that all of the state required to render the page is contained in the stores.  A store keeps a
-- global reference to a value of type @storeData@, which must be an instance of 'StoreData'.
--
-- Stores also work when compiled with GHC instead of GHCJS.  When compiled with GHC, the store is
-- just an MVar containing the store data and there are no controller views.  'alterStore' can still
-- be used, but it just 'transform's the store and does not notify any controller-views since there
-- are none.  Compiling with GHC instead of GHCJS can be helpful for unit testing, although GHCJS
-- plus node can also be used for unit testing.
--
-- >data Todo = Todo {
-- >    todoText :: Text
-- >  , todoComplete :: Bool
-- >  , todoIsEditing :: Bool
-- >} deriving (Show, Typeable)
-- >
-- >newtype TodoState = TodoState {
-- >    todoList :: [(Int, Todo)]
-- >} deriving (Show, Typeable)
-- >
-- >data TodoAction = TodoCreate Text
-- >                | TodoDelete Int
-- >                | TodoEdit Int
-- >                | UpdateText Int Text
-- >                | ToggleAllComplete
-- >                | TodoSetComplete Int Bool
-- >                | ClearCompletedTodos
-- >  deriving (Show, Typeable, Generic, NFData)
-- >
-- >instance StoreData TodoState where
-- >    type StoreAction TodoState = TodoAction
-- >    transform action (TodoState todos) = ...
-- >
-- >todoStore :: ReactStore TodoState
-- >todoStore = mkStore $ TodoState
-- >    [ (0, Todo "Learn react" True False)
-- >    , (1, Todo "Learn react-flux" False False)
-- >    ]
data ReactStore storeData = ReactStore {
    -- | A reference to the foreign javascript part of the store.
    storeRef :: ReactStoreRef storeData

    -- | An MVar containing the current store data.  Normally, the MVar is full and contains the
    -- current store data.  When applying an action, the MVar is kept empty for the entire operation
    -- of transforming to the new data and sending the new data to all component views.  This
    -- effectively operates as a lock allowing only one thread to modify the store at any one time.
    -- This lock is safe because only the 'alterStore' function ever writes this MVar.
  , storeData :: MVar storeData
} deriving (Generic)

-- | Obtain the store data from a store.  Note that the store data is stored in an MVar, so
-- 'getStoreData' can block since it uses 'readMVar'.  The 'MVar' is empty exactly when the store is
-- being transformed, so there is a possiblity of deadlock if two stores try and access each other's
-- data during transformation.
getStoreData :: ReactStore storeData -> IO storeData
getStoreData (ReactStore _ mvar) = readMVar mvar

-- | The data in a store must be an instance of this typeclass.
class Typeable storeData => StoreData storeData where
    -- | The actions that this store accepts
    type StoreAction storeData

    -- | Transform the store data according to the action.  This is the only place in your app where
    -- @IO@ should occur.  The transform function should complete quickly, since the UI will not be
    -- re-rendered until the transform is complete.  Therefore, if you need to perform some longer
    -- action, you should fork a thread from inside 'transform'.  The thread can then call 'alterStore'
    -- with another action with the result of its computation.  This is very common to communicate with
    -- the backend using AJAX.  Indeed, the 'React.Flux.Combinators.jsonAjax' utility function
    -- implements exactly this strategy since it is so common.
    --
    -- Note that if the transform throws an exception, the transform will be aborted and the old
    -- store data will be kept unchanged.  The exception will then be thrown from 'alterStore'.
    --
    -- For the best performance, care should be taken in only modifying the part of the store data
    -- that changed (see below for more information on performance).
    transform :: StoreAction storeData -> storeData -> IO storeData

-- | An existential type for some store action.  It is used as the output of the dispatcher.
-- The 'NFData' instance is important for performance, for details see below.
data SomeStoreAction = forall storeData. (StoreData storeData, NFData (StoreAction storeData))
    => SomeStoreAction (ReactStore storeData) (StoreAction storeData)

instance NFData SomeStoreAction where
    rnf (SomeStoreAction _ action) = action `deepseq` ()

----------------------------------------------------------------------------------------------------
-- mkStore has two versions
----------------------------------------------------------------------------------------------------


-- | Create a new store from the initial data.
unsafeMkStore :: StoreData storeData => storeData -> ReactStore storeData
unsafeMkStore = unsafePerformIO . mkStore
{-# NOINLINE unsafeMkStore #-}

mkStore :: StoreData storeData => storeData -> IO (ReactStore storeData)

#ifdef __GHCJS__

mkStore initial = do
    i <- export initial
    ref <- js_CreateStore i
    storeMVar <- newMVar initial
    return $ ReactStore ref storeMVar

-- | Create the javascript half of the store.
foreign import javascript unsafe
    "{sdata:$1, views: []}"
    js_CreateStore :: Export storeData -> IO (ReactStoreRef storeData)

-- | Perform the update, swapping the old export and the new export and then notifying the component views.
foreign import javascript unsafe
    "hsreact$transform_store($1, $2)"
    js_UpdateStore :: ReactStoreRef storeData -> Export storeData -> IO ()

#else

mkStore initial = do
    storeMVar <- newMVar initial
    return $ ReactStore (ReactStoreRef ()) storeMVar

#endif


----------------------------------------------------------------------------------------------------
-- alterStore has two versions
----------------------------------------------------------------------------------------------------

-- | First, 'transform' the store data according to the given action.  Next, if compiled with GHCJS,
-- notify all registered controller-views to re-render themselves.  (If compiled with GHC, the store
-- data is just transformed since there are no controller-views.)
--
-- Only a single thread can be transforming the store at any one time, so this function will block
-- on an 'MVar' waiting for a previous transform to complete if one is in process.
alterStore :: StoreData storeData => ReactStore storeData -> StoreAction storeData -> IO ()

#ifdef __GHCJS__

alterStore store action = modifyMVar_ (storeData store) $ \oldData -> do
    newData <- transform action oldData
    updateStore' store newData
    return newData

updateStore' store newData = do
    -- There is a hack in PropertiesAndEvents that the fake event store for propagation and prevent
    -- default does not have a javascript store, so the store is nullRef.
    case storeRef store of
        ReactStoreRef ref | not $ isNull ref -> do
            newDataE <- export newData
            js_UpdateStore (storeRef store) newDataE
        _ -> return ()
#else

alterStore store action = modifyMVar_ (storeData store) (transform action)

#endif

modifyStore :: Typeable storeData => ReactStore storeData -> (storeData -> IO (storeData, result)) -> IO result

#ifdef __GHCJS__

modifyStore store action = modifyMVar (storeData store) $ \oldData -> do
    (newData, result) <- action oldData
    updateStore' store newData
    return (newData, result)

#else

modifyStore store action = modifyMVar (storeData store) action

#endif

modifyStoreIf :: Typeable storeData => ReactStore storeData -> (storeData -> Bool) -> (storeData -> IO (storeData, result)) -> (storeData -> IO result) -> IO result

#ifdef __GHCJS__

modifyStoreIf store cond actionTrue actionFalse = modifyMVar (storeData store) $ \oldData -> do
    if cond oldData
        then do
            (newData, result) <- actionTrue oldData
            updateStore' store newData
            return (newData, result)
        else do
            result <- actionFalse oldData
            return (oldData, result)

#else

modifyStoreIf store cond actionTrue actionFalse = modifyMVar (storeData store) $ \oldData -> do
    if cond oldData
        then actionTrue oldData
        else do
            result <- actionFalse oldData
            return (oldData, result)
#endif


-- | Call 'alterStore' on the store and action.
executeAction :: SomeStoreAction -> IO ()
executeAction (SomeStoreAction store action) = alterStore store action
