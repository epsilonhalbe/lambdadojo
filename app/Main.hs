{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (forever)
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import           Commands
import           Twotter
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Attoparsec.Text
import           Data.Monoid ((<>))
import qualified Data.Map as M
import           Data.Map (Map(..), empty)
import           Control.Monad.State

main :: IO ()
main = do TIO.putStrLn "Welcome @Twotter"
          hSetBuffering stdout NoBuffering
          forever (twotter >> putStrLn "------------------------------------")


twotter :: IO ()
twotter = do putStr "> "
             input <- TIO.getLine
             now <- getCurrentTime
             case parseOnly command input
                 of Right c -> print c
                    _       -> TIO.putStrLn "not yet implemented"

test :: Twotter ()
test = do lift $ putStr "> "
          x <- get
          input <- lift TIO.getLine
          now <- lift getCurrentTime
          case parseOnly command input
              of Right c -> lift $ print c
                 _       -> lift $ TIO.putStrLn "not yet implemented"
