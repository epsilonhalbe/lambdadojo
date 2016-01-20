{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Commands where

import           Control.Lens
import           Control.Applicative
import           Data.Attoparsec.Text as A
import           Data.Text as T
import           Data.Char (isSpace)
import           Twotter (Message(..), User, UserName)


data Command = POST {_message :: Message}
             | READ {_userName :: UserName }
             | FOLLOW {_who :: UserName , _whom :: UserName}
             | WALL {_user :: UserName}
             deriving (Show, Eq)
$(makePrisms ''Command)

command :: Parser Command 
command = choice [post_, read_, follow_] --, wall_]
  where post_ :: Parser Command
        post_ = do _author <- T.strip  <$> A.takeWhile (/= '-')
                   string "->"
                   _content <- T.strip <$> takeText
                   return (POST Message{..})
        read_ :: Parser Command
        read_ = do skipSpace
                   _userName <- T.strip <$> takeText
                   return READ{..}
        follow_ :: Parser Command
        follow_ = do _who <- T.strip <$> A.takeWhile (not . isSpace)
                     skipSpace
                     string "follows"
                     skipSpace
                     _whom <- T.strip <$> A.takeWhile (not . isSpace)
                     return FOLLOW{..}
