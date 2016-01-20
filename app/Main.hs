{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forever)
import Commands
import Twotter
import           Data.Text.IO as TIO
import Data.Time.Clock
import Data.Attoparsec.Text

main :: IO ()
main = forever twotter


twotter :: IO ()
twotter = do input <- TIO.getLine
             now <- getCurrentTime
             case parseOnly command input
                 of Right POST{..} -> print $ _message {_timestamp = now}
                    _ -> TIO.putStrLn "not yet implemented"

