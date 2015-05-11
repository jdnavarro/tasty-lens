{-# LANGUAGE FlexibleContexts #-}
module Test.Tasty.SmallCheck.Setter where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial(coseries), Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Setter

testSetter
  :: (Eq s, Show s, Show a, Serial IO a, Serial Identity a, CoSerial IO a)
  => ASetter' s a -> Series IO s -> TestTree
testSetter l se = testGroup "Setter Laws"
  [ testProperty "set l y (set l x a) ≡ set l y a" $
      setterSetSet l se series series
  , testProperty "over l id ≡ id" $ setterId l se
  , testProperty "over l f . over l g ≡ over l (f . g)" $
      setterComposition l se (coseries series) (coseries series)
  ]
