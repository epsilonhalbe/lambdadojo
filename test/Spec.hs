{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text (parseOnly)
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit


import Twotter
import Commands

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [--scprop
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [--qcprop
  ]

unitTests = testGroup "Unit tests"
  [ testGroup "[POST]: 'name' -> 'msg' == POST 'name' msg"
    [ testCase "intended" $
      parseOnly command "Alice -> I love the weather today" @?= (Right $ POST $ Message "Alice" "I love the weather today" undefined)
    , testCase "many space" $
      parseOnly command "Alice   ->   I love the weather today" @?= (Right $ POST $ Message "Alice" "I love the weather today" undefined)
    , testCase "no space" $
      parseOnly command "Alice->I love the weather today" @?= (Right $ POST $ Message "Alice" "I love the weather today" undefined)
    ]
  , testGroup "[READ]: reading 'name' == READ 'name'"
    [ testCase "intended" $
      parseOnly command "Alice" @?= (Right $ READ "Alice")
    , testCase "many space" $
      parseOnly command "   Alice   " @?= (Right $ READ "Alice")
    ]
  --hunit
  ]
