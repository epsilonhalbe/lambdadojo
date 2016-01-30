{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Twotter where

import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Lens
import           Data.Time
import           Data.Monoid
import           Data.Map (Map(..))
import           Control.Monad.State

type UserName = Text

data Message = Message { _author :: UserName
                       , _content :: Text
                       } deriving (Eq)
$(makeLenses ''Message)

instance Show Message where
    show message = T.unpack $ message^.author <> " -> " <> message^.content


data User = User { _userName :: UserName
                 , _followers :: [User]
                 , _following :: [User]
                 , _messages  :: [(UTCTime, Message)]
                 } deriving (Show, Eq)

$(makeLenses ''User)

type Twotter = State (Map UserName User)

displayMessage :: UTCTime -> (UTCTime, Message) -> String
displayMessage now (timestamp, msg) = show msg <> showTime (diffUTCTime now timestamp)

showTime :: NominalDiffTime -> String
showTime s
       | s < 0*min  = error "future messages are not accepted - yet"
       | s < 1*min  = "(now)"
       | s < 2*min  = unwords ["(",show . floor $ s / min,"min ago )"]
       | s < 1*hour = unwords ["(",show . floor $ s / min,"mins ago )"]
       | s < 2*hour = unwords ["(",show . floor $ s / hour,"hour ago )"]
       | s < 1*day  = unwords ["(",show . floor $ s / hour,"hours ago )"]
       | s < 2*day  = unwords ["(",show . floor $ s / day,"day ago ) "]
       | s < 7*day  = unwords ["(",show . floor $ s / day,"days ago )"]
       | otherwise  = "( a long time ago )"
       where min  = 60
             hour = 60*min
             day  = 24*hour

