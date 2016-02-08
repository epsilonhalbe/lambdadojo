{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import           Commands
import           Twotter
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Attoparsec.Text
import           Data.Monoid
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import           Control.Monad.State
import           Control.Lens


main :: IO ()
main = do TIO.putStrLn "Welcome @Twotter"
          hSetBuffering stdout NoBuffering
          forever (evalStateT twotter M.empty >> putStrLn "------------------------------------")

twotter :: Twotter ()
twotter = forever $
  do lift $ putStr "> "
     input <- lift TIO.getLine
     now <- lift getCurrentTime
     case parseOnly command input
       of Right POST{..} ->
                    do let u = mempty & messages .~ M.singleton now _message
                                      & userName .~ (_message^.author)
                       modify $ M.insertWith (<>) (_message^.author) u

          Right READ{..} ->
                    do x <- gets $ M.lookup _userName
                       lift $ maybe (TIO.putStrLn $ _userName <> " does not exist.")
                                    (M.foldMapWithKey (\k v -> putStrLn $ displayMessage now k v))
                                    (_messages <$> x)

          Right WALL{..} ->
                    do me <- gets $ M.lookup _userName
                       x  <- get
                       let myFollowers  = maybe S.empty _following me :: Set UserName
                           -- TODO: Some explanations what this does and why it works
                           wall = let (∪) :: Maybe User -> Map UTCTime Message -> Map UTCTime Message
                                      (Just a) ∪ b = M.union (_messages a) b
                                      Nothing  ∪ b = b
                                  in foldr (∪) M.empty $
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
                               _ -> do let whoU = mempty & userName .~ _who
                                                         & following .~ S.singleton _whom
                                           whomU = mempty & userName .~ _whom
                                                          & followers .~ S.singleton _who
                                       modify $ M.insertWith (<>) _who whoU
                                       modify $ M.insertWith (<>) _whom whomU

          _       -> lift $ TIO.putStrLn "not yet implemented"


instance Monoid (IO ()) where
    mempty = return ()
    mappend = (>>)
