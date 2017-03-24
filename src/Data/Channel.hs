module Data.Channel
    ( Channel
    , subscribe
    , send
    , newChannel
    , reverseChannel
    , dupChannel
    ) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Monad.Trans
import Control.Monad (forever)

import qualified Pipes as P


data Channel a b = Channel { channelIn :: TChan a
                           , channelOut :: TChan b }

subscribe :: MonadIO m => Channel a b -> (a -> m ()) -> m ()
subscribe channel f = forever $ do
    a <- liftIO $ atomically $ readTChan $ channelIn channel
    f a

requestEvent :: MonadIO m => Channel a b -> m a
requestEvent = liftIO . atomically . readTChan . channelIn

send :: MonadIO m => Channel a b -> b -> m ()
send channel b =
    let o = channelOut channel
    in liftIO $ atomically $ writeTChan o b

reverseChannel :: Channel a b -> Channel b a
reverseChannel (Channel a b) = Channel b a

newChannel :: MonadIO m => m (Channel a b)
newChannel = liftIO $ Channel <$> atomically newTChan <*> atomically newTChan

dupChannel :: MonadIO m => Channel a b -> m (Channel a b)
dupChannel (Channel i o) = do
    i' <- liftIO $ atomically $ dupTChan i
    return $ Channel i' o
