{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Commands where

import           Control.Lens
import           Control.Applicative
import           Data.Attoparsec.Text as A
import qualified Data.Text as T
import           Data.Char (isSpace)
import           Twotter (Message(..), User, UserName)


data Command = POST {_message :: Message}
             | READ {_userName :: UserName }
             | FOLLOW {_who :: UserName , _whom :: UserName}
             | WALL {_userName :: UserName}
             deriving (Show, Eq)
$(makePrisms ''Command)

command :: Parser Command
command = choice $ map (skipSpace >>)[post_, wall_, follow_, read_]
  where post_ :: Parser Command
        post_ = do _author <- T.strip  <$> A.takeWhile (/= '-')
                   string "->"
                   _content <- T.strip <$> takeText
                   return (POST Message{..})
        read_ :: Parser Command
        read_ = do _userName <- T.strip <$> takeText
                   return READ{..}
        wall_ :: Parser Command
        wall_ = do _userName <- T.strip <$> A.takeWhile (not . isSpace)
                   skipSpace
                   string "wall"
                   return WALL{..}
        follow_ :: Parser Command
        follow_ = do _who <- T.strip <$> A.takeWhile (not . isSpace)
                     skipSpace
                     string "follows"
                     skipSpace
                     _whom <- T.strip <$> A.takeWhile (not . isSpace)
                     return FOLLOW{..}
