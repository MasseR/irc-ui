module Data.Channel (Channel) where

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
