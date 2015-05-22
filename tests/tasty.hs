module Main where

import Control.Lens
import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.SmallCheck.Lens

main :: IO ()
main = defaultMain $ testGroup "Lens tests"
  [ tupleTests
  ]

tupleTests :: TestTree
tupleTests = testGroup "Tuples"
  [ testGroup "_1 :: Lens' (Char,Char) Char"
    [ testLens (_1 :: Lens' (Char,Char) Char) ]
  , testGroup "_2 :: Lens' (Char,Char,Char) Char"
    [ testLens (_2 :: Lens' (Char,Char,Char) Char) ]
  ]
