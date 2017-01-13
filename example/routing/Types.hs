{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

-- |

module Types where

import           React.Flux


import qualified Data.Text     as T
import           Data.Typeable (Typeable)
import qualified Data.JSString.Text as JSS

type AppName = T.Text
type AppView = ReactElementM ViewEventHandler
type AppRouter = [T.Text] -> IO ()

data App props = forall state. (StoreData state, Eq state) =>
           App {appName        :: AppName
               , appState      :: ReactStore state
               , appView       :: Typeable props => state -> props -> AppView ()
               , appInitAction :: StoreAction state
               , appRouter     :: Maybe AppRouter
               }
               deriving Typeable

initApp :: (Typeable props, Eq props) => App props -> IO (ReactView props)
initApp App{..} = do
  let view' = defineControllerView (JSS.textToJSString appName) appState (\st props -> appView st props)
  alterStore appState appInitAction
  return view'
