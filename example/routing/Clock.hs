{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- |

module Clock where

import           React.Flux         hiding (view)
import qualified React.Flux         as RF

import           Control.Concurrent (forkIO, threadDelay)
import           Control.DeepSeq
import           Control.Monad      (void)
import           Data.Time          (UTCTime, getCurrentTime)
import           Data.Typeable      (Typeable)
import           GHC.Generics       (Generic)


data ClockState = ClockState {csTime :: !(Maybe UTCTime)
                             }
                deriving (Show, Typeable, Eq)

data ClockAction = ClockInit
                 | ClockTick UTCTime
                 deriving (Show, Typeable, Generic, NFData)

instance StoreData ClockState where
  type StoreAction ClockState = ClockAction
  transform action st =
    -- putStrLn $ "Action: " ++ show action
    case action of
      ClockInit -> do
        void $ forkIO $ workerLoop store
        return st
      ClockTick tm ->
        return $ st{csTime = Just tm}
    where
      workerLoop rst = do
        storeWorker rst
        workerLoop rst
      storeWorker rst = do
        threadDelay 1000000
        tm <- getCurrentTime
        alterStore rst (ClockTick tm)

store :: ReactStore ClockState
store = unsafeMkStore $ ClockState Nothing

view :: ReactView ClockState
view = defineView "clock" $ \st ->
  div_ $  maybe mempty elemShow $ csTime st

view_ :: ClockState -> ReactElementM eventHandler ()
view_ st =
  RF.view view st mempty
