module Data.Channel
    ( Channel
    , subscribe
    , send
    , newChannel
    ) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM (atomically)
import Control.Monad.Trans
import Control.Monad (forever)


data Channel a b = Channel { channelIn :: TChan a
                           , channelOut :: TChan b }

subscribe :: MonadIO m => Channel a b -> (a -> m ()) -> m ()
subscribe channel f = forever $ do
    a <- liftIO $ atomically $ readTChan $ channelIn channel
    f a

send :: MonadIO m => Channel a b -> b -> m ()
send channel b =
    let o = channelOut channel
    in liftIO $ atomically $ writeTChan o b

newChannel :: MonadIO m => m (Channel a b)
newChannel = liftIO $ Channel <$> atomically newTChan <*> atomically newTChan
