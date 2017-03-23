{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}
module Data.IRC where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Aeson
import GHC.Generics
import Data.String
import Control.Lens
import Data.SafeCopy
import Data.Time
import Data.ByteString.Char8
import Data.Monoid
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text (Parser)
import qualified Data.Text as T
import Control.Applicative

newtype Channel = Channel Text deriving (Generic, Show, ToJSON, FromJSON, IsString, Ord, Eq)
newtype Nick = Nick Text deriving (Generic, Show, ToJSON, FromJSON, IsString, Ord, Eq)

data Message = Message { messageTimestamp :: UTCTime
                       , messageContent :: Text }
             deriving (Show, Generic)

data IrcMessage = IrcMessage { ircMessageSource :: Either Channel Nick
                             , ircMessageMessage :: Message }
                deriving(Show, Generic)

data IrcJoin = IrcJoin { ircJoinChannel :: Channel
                       , ircJoinNick :: Nick }
             deriving(Show, Generic)

newtype IrcPong = IrcPong Text deriving (Show, Generic)

data IrcEvent = EventMessage IrcMessage
              | EventJoin IrcJoin
              | EventPing Text
              | EventPong IrcPong
              | EventNick Nick
              | EventUser Nick
              deriving (Generic, Show)

class ToIRC a where
    encodeIRC :: a -> ByteString

instance ToIRC IrcPong where
    encodeIRC (IrcPong p) = "PONG " <> TE.encodeUtf8 p

makeFields ''IrcMessage
makeFields ''Message
makeFields ''IrcJoin
makePrisms ''IrcEvent
deriveSafeCopy 0 'base ''Nick
deriveSafeCopy 0 'base ''Channel
deriveSafeCopy 0 'base ''Message


instance ToJSON Message
instance FromJSON Message
instance ToJSON IrcPong
instance FromJSON IrcPong
instance ToJSON IrcJoin
instance FromJSON IrcJoin
instance ToJSON IrcMessage
instance FromJSON IrcMessage
instance ToJSON IrcEvent
instance FromJSON IrcEvent

parseIRC :: Parser IrcEvent
parseIRC = EventPing <$> parsePing

parsePing :: Parser Text
parsePing = A.string "PING: " *> A.takeText

parseSource :: Parser (Either Data.IRC.Channel Nick)
parseSource = (Left . Data.IRC.Channel) <$> channel
    <|> (Right . Nick) <$> nick
    where
        channel = T.cons <$> A.char '#' <*> A.takeWhile1 (/= ' ')
        nick = A.takeWhile1 (/= ' ')
