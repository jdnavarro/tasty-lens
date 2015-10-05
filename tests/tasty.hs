module Main where

import Control.Lens
import Numeric.Lens (binary, negated, adding)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (allowFail)
import Test.DumbCheck -- (TestTree, defaultMain, testGroup)

import qualified Test.Tasty.Lens.Iso as Iso
import qualified Test.Tasty.Lens.Lens as Lens
import qualified Test.Tasty.Lens.Prism as Prism

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
      [ Lens.test (_1 :: Lens' (Char,Char) Char) ]
    , testGroup "Lens' (Int,Char) Int"
      [ Lens.test (_1 :: Lens' (Int,Char) Int) ]
    ]
  , testGroup "_2"
     [ testGroup "Lens' (Char,Char,Char) Char"
       [ Lens.test (_2 :: Lens' (Char,Char,Char) Char) ]
     ]
  ]

maybeTests :: TestTree
maybeTests = testGroup "Maybe"
  [ testGroup "_Nothing"
    [ testGroup "Prism' (Maybe Char) ()"
      [ Prism.test (_Nothing :: Prism' (Maybe Char) ()) ]
    , testGroup "Prism' (Maybe Int) ()"
      [ Prism.test (_Nothing :: Prism' (Maybe Int) ()) ]
    ]
  , testGroup "_Just"
    [ testGroup "Prism' (Maybe Char) Char"
      [ Prism.test (_Just :: Prism' (Maybe Char) Char) ]
    , testGroup "Prism' (Maybe Int) Int"
      [ Prism.test (_Just :: Prism' (Maybe Int) Int) ]
    ]
  ]

type AlphaNumString = [AlphaNum]

numTests :: TestTree
numTests = testGroup "Numeric"
  [ testGroup "negated"
    [ testGroup "Iso' Int Int"
      [ Iso.test (negated :: Iso' Int Int) ]
    , testGroup "Iso' Float Float"
      [ Iso.test (negated :: Iso' Float Float) ]
    ]
  , testGroup "binary"
    [ testGroup "Prism'' String Int"
      [ allowFail $ Prism.test
                  (_AlphaNumString . (binary :: Prism' String Int)) ]
    , testGroup "Prism'' String Integer"
      [ Prism.test (binary :: Prism' String Integer) ]
    ]
  , testGroup "adding"
    [ testGroup "Iso' Int Int"
      [ Iso.test (adding 2 :: Iso' Int Int) ]
    , testGroup "Iso' Integer Integer"
      [ Iso.test (adding 3 :: Iso' Integer Integer) ]
    ]
  ]
  where
    _AlphaNumString :: Iso' AlphaNumString String
    _AlphaNumString = iso (fmap unAlphaNum) (fmap AlphaNum)
