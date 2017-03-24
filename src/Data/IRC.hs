{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
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
                       , messageSource :: Either Channel Nick
                       , messageContent :: Text }
             deriving (Show, Generic)

data IrcJoin = IrcJoin { ircJoinChannel :: Channel
                       , ircJoinNick :: Nick }
             deriving(Show, Generic)

newtype Pong = Pong Text deriving (Show, Generic, ToJSON, FromJSON)

newtype Ping = Ping Text deriving (Show, Generic, ToJSON, FromJSON)

data IrcUser = IrcUser { ircUserUser :: Text
                       , ircUserMode :: Text
                       , ircUserRealname :: Text }
             deriving (Show, Generic)

data InboundEvent = InboundMessage Message
                  | InboundPing Ping
                  deriving (Generic, Show)

class ToIRC a where
    encodeIRC :: a -> ByteString

instance ToIRC Pong where
    encodeIRC (Pong p) = "PONG " <> TE.encodeUtf8 p

instance ToJSON InboundEvent
instance ToJSON Message

makeFields ''Message
makeFields ''IrcJoin
makePrisms ''InboundEvent
deriveSafeCopy 0 'base ''Nick
deriveSafeCopy 0 'base ''Channel
deriveSafeCopy 0 'base ''Message


-- parseIRC :: Parser InboundEvent
-- parseIRC = InboundPing <$> parsePing
--
-- parsePing :: Parser Text
-- parsePing = A.string "PING: " *> A.takeText
--
-- parseSource :: Parser (Either Data.IRC.Channel Nick)
-- parseSource = (Left . Data.IRC.Channel) <$> channel
--     <|> (Right . Nick) <$> nick
--     where
--         channel = T.cons <$> A.char '#' <*> A.takeWhile1 (/= ' ')
--         nick = A.takeWhile1 (/= ' ')
