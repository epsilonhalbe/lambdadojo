{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (forever)
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import           Commands
import           Twotter
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Attoparsec.Text
import           Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Map (Map(..), empty)
import           Control.Monad.State
import           Control.Lens
import           Data.Maybe (maybe)
import           Data.Foldable (traverse_)



main :: IO ()
main = do TIO.putStrLn "Welcome @Twotter"
          hSetBuffering stdout NoBuffering
          -- forever (twotter >> putStrLn "------------------------------------")
          forever (evalStateT test empty >> putStrLn "------------------------------------")


twotter :: IO ()
twotter = do putStr "> "
             input <- TIO.getLine
             now <- getCurrentTime
             case parseOnly command input
                 of Right c -> print c
                    _       -> TIO.putStrLn "not yet implemented"

test :: Twotter ()
test = forever $
    do lift $ putStr "> "
       input <- lift TIO.getLine
       now <- lift getCurrentTime
       case parseOnly command input
           of Right c@POST{..} ->
                 do lift $ print c
                    let u = mempty & messages .~ M.singleton now _message
                    modify $ M.insertWith (<>) (_message^.author) u
              Right c@READ{..} ->
                 do x <- gets $ M.lookup _userName
                    lift $ maybe (TIO.putStrLn $ _userName <> " does not exist.")
                                 (M.foldMapWithKey (\k v -> putStrLn $ displayMessage now k v))
                                 {-(M.foldlWithKey (\_ k v-> putStrLn $ displayMessage now k v) (return ()))-}
                                 (_messages <$> x)
              Right c@WALL{..} ->
                 fail "not implemented"
              Right c -> lift $ print c
              _       -> lift $ TIO.putStrLn "not yet implemented"

instance Monoid (IO ()) where
    mempty = return ()
    mappend = (>>)
