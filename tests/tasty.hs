module Main where

import Control.Lens
import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.SmallCheck.Lens
import Test.Tasty.SmallCheck.Prism

main :: IO ()
main = defaultMain $ testGroup "Lens tests"
  [ tupleTests
  , maybeTests
  ]

tupleTests :: TestTree
tupleTests = testGroup "Tuples"
  [ testGroup "_1 :: Lens' (Char,Char) Char"
    [ testLens (_1 :: Lens' (Char,Char) Char) ]
  , testGroup "_2 :: Lens' (Char,Char,Char) Char"
    [ testLens (_2 :: Lens' (Char,Char,Char) Char) ]
  ]

maybeTests :: TestTree
maybeTests = testGroup "Maybe"
  [ testGroup "_Nothing"
    [ testGroup "Prism' (Maybe Char) ()"
      [ testPrism (_Nothing :: Prism' (Maybe Char) ()) ]
    , testGroup "Prism' (Maybe Int) ()"
      [ testPrism (_Nothing :: Prism' (Maybe Int) ()) ]
    ]
  , testGroup "_Just"
    [ testGroup "Prism' (Maybe Char) Char"
      [ testPrism (_Just :: Prism' (Maybe Char) Char) ]
    , testGroup "Prism' (Maybe Int) Int"
      [ testPrism (_Just :: Prism' (Maybe Int) Int) ]
    ]
  ]
