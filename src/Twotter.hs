{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RecordWildCards #-}

module Twotter where

import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Lens
import           Data.Time
import           Data.Function (on)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Map (Map)
import           Control.Monad.State


type UserName = Text

data Message = Message { _author :: UserName
                       , _content :: Text
                       } deriving (Eq)
$(makeLenses ''Message)

instance Show Message where
    show message = T.unpack $ message^.author <> " -> " <> message^.content

data User = User { _userName :: UserName
                 , _followers :: Set UserName
                 , _following :: Set UserName
                 , _messages  :: Map UTCTime Message
                 } deriving (Show, Eq)

$(makeLenses ''User)

instance Ord User where
    compare = compare `on` _userName

instance Monoid User where
    mappend u1 u2 | u1^.userName == u2^.userName = u1 & followers %~ S.union (u2^.followers)
                                                      & following %~ S.union (u2^.following)
                                                      & messages  %~ M.union (u2^.messages )

                  | otherwise = u1
    mempty = User "" S.empty S.empty M.empty

type Twotter = StateT (Map UserName User) IO

displayMessage :: UTCTime -> UTCTime -> Message -> String
displayMessage now timestamp msg = unwords [T.unpack $ msg^.content, showTime (diffUTCTime now timestamp)]

showTime :: NominalDiffTime -> String
showTime s
       | s < 0*min' = error "future messages are not accepted - yet"
       | s < 1*min' = "(now)"
       | s < 2*min' = unwords ["(",show' $ s / min',"min ago )"]
       | s < 1*hour = unwords ["(",show' $ s / min',"mins ago )"]
       | s < 2*hour = unwords ["(",show' $ s / hour,"hour ago )"]
       | s < 1*day  = unwords ["(",show' $ s / hour,"hours ago )"]
       | s < 2*day  = unwords ["(",show' $ s / day,"day ago ) "]
       | s < 7*day  = unwords ["(",show' $ s / day,"days ago )"]
       | otherwise  = "( a long time ago )"
       where min' = 60 :: NominalDiffTime
             hour = 60*min'
             day  = 24*hour
             show' = show . (floor :: RealFrac a => a -> Int)

