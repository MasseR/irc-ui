{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
module Data.User where

import Data.IRC (Nick, Channel, Message)
import Data.Text (Text)
import Control.Lens
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.SafeCopy
import Data.Sequence (Seq)
import Data.Map.Strict (Map)

data User = User { userEmail :: Text
                 , userRealname :: Text
                 , userPreferredNick :: Nick
                 , userPassword :: ByteString
                 , userChannels :: Set Channel
                 , userHistory :: Map Channel Message
                 }
          deriving (Show)

deriveSafeCopy 0 'base ''User

makeFields ''User
