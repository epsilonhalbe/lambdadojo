{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Set (Set(..))
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
           of Right c@POST{..}   ->
                  do let u = mempty & messages .~ M.singleton now _message
                                    & userName .~ (_message^.author)
                     modify $ M.insertWith (<>) (_message^.author) u

              Right c@READ{..}   ->
                  do x <- gets $ M.lookup _userName
                     lift $ maybe (TIO.putStrLn $ _userName <> " does not exist.")
                                  (M.foldMapWithKey (\k v -> putStrLn $ displayMessage now k v))
                                  {-(M.foldlWithKey (\_ k v-> putStrLn $ displayMessage now k v) (return ()))-}
                                  (_messages <$> x)

              Right c@WALL{..}   -> 
                  do me <- gets $ M.lookup _userName
                     x  <- get
                     let myMsg = maybe M.empty  _messages me
                         myFollowers  = maybe S.empty _following me :: Set UserName
                         wall = foldr f empty $
                                S.insert me $ S.map (`M.lookup` x) myFollowers
                                           :: Map UTCTime Message
                     lift $ M.foldMapWithKey (\k v -> putStrLn $ unwords
                                                               [ T.unpack $ v^.author
                                                               , "-"
                                                               , displayMessage now k v]) wall

              Right c@FOLLOW{..} ->
                  do lift $ print c
                     whom' <- gets $ M.lookup _whom
                     case whom' of
                         Nothing  -> lift $ TIO.putStrLn
                                          $ T.unwords ["User:", _whom, "does not exist."]
                         _ -> do let who' = mempty & userName .~ _who
                                                   & following .~ S.singleton _whom 
                                     whom' = mempty & userName .~ _whom
                                                    & followers .~ S.singleton _who
                                 modify $ M.insertWith (<>) _who who'
                                 modify $ M.insertWith (<>) _whom whom'

              _       -> lift $ TIO.putStrLn "not yet implemented"

  where f (Just x) y = M.union (_messages x) y
        f _ y = y

instance Monoid (IO ()) where
    mempty = return ()
    mappend = (>>)
