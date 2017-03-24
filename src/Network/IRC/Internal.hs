{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
module Network.IRC.Internal where

-- import qualified Network.Simple.TCP as TCP
import qualified Pipes.Network.TCP as P
import qualified Pipes.Text.Encoding as PTE
import qualified Pipes.Text as PT
import qualified Pipes.Prelude.Text as PT
import qualified Pipes.Group as P
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser)
import Pipes
import Data.Channel as C
import Control.Lens
import Control.Concurrent (forkIO, killThread, ThreadId)
import Data.IRC
import Control.Applicative
import qualified Data.User as U
import Say

connect :: U.User -> P.HostName -> P.ServiceName -> IO (C.Channel InboundEvent OutboundEvent, IO ())
connect user host port = do
    (socket, _) <- P.connectSock host port
    channel <- newChannel
    parser <- forkIO $ void $ runEffect $ for (producer socket) (lift . either sayErrString (C.send (reverseChannel channel)))
    reader <- forkIO $ runEffect $ (subscribe (reverseChannel channel) yield) >-> consumer socket
    pingHandler <- C.dupChannel channel >>= forkIO . pinger
    mapM_ (send channel) initial
    return (channel, kill [parser, reader, pingHandler] socket)
    where
        outNick = OutboundNick (view U.preferredNick user)
        outUser = OutboundUser $ User (view (U.preferredNick . _Nick) user) "0" (view U.realname user)
        initial = [outNick, outUser] :: [OutboundEvent]
        kill :: [ThreadId] -> P.Socket -> IO ()
        kill ts s = mapM_ killThread ts >> P.closeSock s
        pinger channel = C.subscribe channel $ \x ->
            case x of
                 InboundPing (Ping p) -> C.send channel (OutboundPong (Pong p))
                 _x -> sayShow x
        subscribeToPipe channel = subscribe

producer socket = (P.concats . view PT.lines . PTE.decodeUtf8 $ P.fromSocket socket 4096)
    >-> P.map T.strip
    >-> P.map (A.parse (parseIRC <* A.endOfInput))
    >-> P.map (flip A.feed "")
    >-> P.map (eitherResult)
    where
        eitherResult (A.Done _ r) = Right r
        eitherResult (A.Fail r _ x) = Left (show r ++ "> " ++ x)
        eitherResult _ = Left "asd"
consumer :: P.Socket -> Consumer OutboundEvent IO ()
consumer socket = P.map toIRC
    >-> P.map (\x -> B.append x "\r\n")
    >-> P.mapM (\x -> sayShow x >> return x)
    >-> P.toSocket socket

