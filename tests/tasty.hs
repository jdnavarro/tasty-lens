module Main where

import Control.Lens
import Numeric.Lens (hex, negated, adding)
import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.SmallCheck.Lens

main :: IO ()
main = defaultMain $ testGroup "Lens tests"
  [ tupleTests
  , maybeTests
  , numTests
  ]

tupleTests :: TestTree
tupleTests = testGroup "Tuples"
  [ testGroup "_1"
    [ testGroup "Lens' (Char,Char) Char"
        [ testLens (_1 :: Lens' (Char,Char) Char) ]
    , testGroup "Lens' (Int,Char) Int"
        [ testLens (_1 :: Lens' (Int,Char) Int) ]
    ]
  , testGroup "_2"
     [ testGroup "Lens' (Char,Char,Char) Char"
         [ testLens (_2 :: Lens' (Char,Char,Char) Char) ]
     ]
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

numTests :: TestTree
numTests = testGroup "Numeric"
  [ testGroup "negated"
    [ testGroup "Iso' Int Int"
      [ testIso (negated :: Iso' Int Int) ]
    , testGroup "Iso' Float Float"
      [ testIso (negated :: Iso' Float Float) ]
    ]
  , testGroup "hex"
    [ testGroup "Prism'' String Int"
      [ testPrism (hex :: Prism' String Int) ]
    , testGroup "Prism'' String Integer"
      [ testPrism (hex :: Prism' String Integer) ]
    ]
  , testGroup "adding"
    [ testGroup "Iso' Int Int"
      [ testIso (adding 2 :: Iso' Int Int) ]
    , testGroup "Iso' Integer Integer"
      [ testIso (adding 3 :: Iso' Integer Integer) ]
    ]
  ]
