{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [--scprop
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [--qcprop
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "[POST]: 'name' -> 'msg' == POST 'name' msg"
    [ testCase "intended" $
      parseOnly command "Alice -> I love the weather today" @?= (Right $ POST $ Message "Alice" "I love the weather today")
    , testCase "many space" $
      parseOnly command "Alice   ->   I love the weather today" @?= (Right $ POST $ Message "Alice" "I love the weather today")
    , testCase "no space" $
      parseOnly command "Alice->I love the weather today" @?= (Right $ POST $ Message "Alice" "I love the weather today")
    ]
  , testGroup "[READ]: reading 'name' == READ 'name'"
    [ testCase "intended" $
      parseOnly command "Alice" @?= (Right $ READ "Alice")
    , testCase "many space" $
      parseOnly command "   Alice   " @?= (Right $ READ "Alice")
    ]
  , testGroup "[FOLLOW]: 'Alice' follows 'Bob' == FOLLOW Alice Bob"
    [ testCase "intended" $
      parseOnly command "Alice follows Bob" @?= (Right $ FOLLOW "Alice" "Bob")
    , testCase "many space" $
      parseOnly command "Alice         follows      Bob" @?= (Right $ FOLLOW "Alice" "Bob")
    ]
  , testGroup "[WALL]: Alice wall == WALL Alice"
    [ testCase "intended" $
      parseOnly command "Alice wall" @?= (Right $ WALL "Alice")
    , testCase "many space" $
      parseOnly command "Alice         wall" @?= (Right $ WALL "Alice")
    ]
  --hunit
  ]
