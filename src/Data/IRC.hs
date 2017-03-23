{-# Language DeriveGeneric #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}
module Data.IRC where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Data.String
import Control.Lens
import Data.SafeCopy
import Data.Time

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

data IrcEvent = PrivateMessage IrcMessage
              | Join IrcJoin
              | Ping Message
              | Pong Message
              deriving (Generic, Show)

makeFields ''IrcMessage
makeFields ''Message
makeFields ''IrcJoin
makePrisms ''IrcEvent
deriveSafeCopy 0 'base ''Nick
deriveSafeCopy 0 'base ''Channel
deriveSafeCopy 0 'base ''Message


instance ToJSON Message
instance FromJSON Message
instance ToJSON IrcJoin
instance FromJSON IrcJoin
instance ToJSON IrcMessage
instance FromJSON IrcMessage
instance ToJSON IrcEvent
instance FromJSON IrcEvent
