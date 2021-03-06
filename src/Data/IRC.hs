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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text (Parser)
import qualified Data.Text as T
import Control.Applicative

newtype Channel = Channel {fromChannel :: Text} deriving (Generic, Show, ToJSON, FromJSON, IsString, Ord, Eq)
newtype Nick = Nick {fromNick :: Text} deriving (Generic, Show, ToJSON, FromJSON, IsString, Ord, Eq)
newtype Pong = Pong Text deriving (Show, Generic, ToJSON, FromJSON)
newtype Ping = Ping Text deriving (Show, Generic, ToJSON, FromJSON)

data Message = Message { messageFrom :: Nick
                       , messageTo :: Either Channel Nick
                       , messageContent :: Text }
             deriving (Show, Generic)

data Join = Join { joinChannel :: Channel
                 , joinNick :: Nick }
          deriving(Show, Generic)

data User = User { userUser :: Text
                 , userMode :: Text
                 , userRealname :: Text }
             deriving (Show, Generic)

data NickChange = NickChange { nickChangeFrom :: Nick
                             , nickChangeTo :: Nick }
                deriving (Show, Generic)

data InboundEvent = InboundMessage Message
                  | InboundPing Ping
                  | InboundMode Text
                  | InboundNick NickChange
                  deriving (Generic, Show)

data OutboundEvent = OutboundMessage Message
                   | OutboundPong Pong
                   | OutboundNick Nick
                   | OutboundUser User
                   | OutboundJoin Join
                   deriving (Generic, Show)

toIRC :: OutboundEvent -> ByteString
toIRC (OutboundMessage msg) = encodeIRC msg
toIRC (OutboundPong msg) = encodeIRC msg
toIRC (OutboundNick msg) = encodeIRC msg
toIRC (OutboundUser msg) = encodeIRC msg
toIRC (OutboundJoin msg) = encodeIRC msg

class ToIRC a where
    encodeIRC :: a -> ByteString

instance ToIRC Pong where
    encodeIRC (Pong p) = "PONG " <> TE.encodeUtf8 p

instance ToIRC User where
    encodeIRC (User u m r) = B.unwords $ map TE.encodeUtf8 ["USER", u, m, "unused", fromName r]
        where fromName = T.cons ':'

instance ToIRC Nick where
    encodeIRC (Nick n) = B.unwords $ map TE.encodeUtf8 ["NICK", n]

instance ToIRC Message where
    encodeIRC (Message _f t m) = B.unwords $ map TE.encodeUtf8 ["PRIVMSG", fromSource t, fromMessage m]
        where
            fromSource = either fromChannel fromNick
            fromMessage = T.cons ':'

instance ToIRC Join where
    encodeIRC (Join c _) = B.unwords $ map TE.encodeUtf8 ["JOIN", fromChannel c]

instance ToJSON InboundEvent
instance ToJSON Message
instance ToJSON NickChange

instance FromJSON OutboundEvent
instance FromJSON Message
instance FromJSON User
instance FromJSON Join

makeFields ''Message
makeFields ''Join
makeFields ''User
makeFields ''NickChange
makePrisms ''InboundEvent
makePrisms ''OutboundEvent
makePrisms ''Nick
deriveSafeCopy 0 'base ''Nick
deriveSafeCopy 0 'base ''Channel
deriveSafeCopy 0 'base ''Message


parseIRC :: Parser InboundEvent
parseIRC = InboundPing <$> parsePing
    <|> InboundMessage <$> parsePrivMsg

parsePing :: Parser Ping
parsePing = Ping <$> (A.string "PING :" *> A.takeText <* A.endOfInput)

parsePrivMsg :: Parser Message
parsePrivMsg = Message <$> parseFrom <* A.string " PRIVMSG " <*> parseTo <*> parseContent
    where
        parseFrom = Nick <$> (A.char ':' *> A.takeWhile1 (/= '!') <* A.takeWhile1 (/= ' '))
        parseTo = (Left . Channel <$> parseChannel) <|> (Right . Nick <$> A.takeWhile1 (/= ' '))
        parseChannel = T.cons <$> A.char '#' <*> A.takeWhile (/= ' ')
        parseContent = A.string " :" *> A.takeText
