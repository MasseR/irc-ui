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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser)
import Pipes
import Data.Channel
import Control.Lens
import Control.Concurrent (forkIO)
import Data.IRC
import Control.Applicative

connect host port = P.connect host port $ \(socket, _) -> do
    channel <- newChannel
    runEffect $ for (producer socket >-> P.map (A.parse (parseIRC <* A.endOfInput))) (lift . print . eitherResult)
    return channel
    where
        eitherResult (A.Done _ r) = Right r
        eitherResult (A.Fail r _ x) = Left (show r ++ "> " ++ x)
        eitherResult _ = Left "asd"

producer socket = (P.concats . view PT.lines . PTE.decodeUtf8 $ P.fromSocket socket 4096) >-> P.map T.strip
consumer socket = P.toSocket socket

