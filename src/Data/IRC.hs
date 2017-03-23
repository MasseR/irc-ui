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

newtype Channel = Channel Text deriving (Generic, Show, ToJSON, FromJSON, IsString)
newtype Nick = Nick Text deriving (Generic, Show, ToJSON, FromJSON, IsString)
newtype Message = Message Text deriving (Generic, Show, ToJSON, FromJSON, IsString)

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
makePrisms ''IrcEvent


instance ToJSON IrcJoin
instance FromJSON IrcJoin
instance ToJSON IrcMessage
instance FromJSON IrcMessage
instance ToJSON IrcEvent
instance FromJSON IrcEvent
