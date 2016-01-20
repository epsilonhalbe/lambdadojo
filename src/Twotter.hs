{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Twotter where

import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Lens
import           Data.Time
import           Data.Monoid

type UserName = Text

data User = User { _userName :: UserName
                 , _followers :: [User]
                 , _following :: [User]
                 } deriving (Show, Eq)

$(makeLenses ''User)

data Message = Message { _author :: UserName
                       , _content :: Text
                       , _timestamp :: UTCTime
                       }
$(makeLenses ''Message)

instance Eq Message where
    msg1 == msg2 = msg1^.author == msg2^.author &&  msg1^.content == msg2^.content

instance Show Message where
    show message = T.unpack $ message^.author <> " -> " <> message^.content

displayMessage :: UTCTime -> Message -> String
displayMessage = undefined
