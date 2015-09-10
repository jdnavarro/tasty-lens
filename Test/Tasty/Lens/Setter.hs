{-# LANGUAGE FlexibleContexts #-}
module Test.Tasty.Lens.Setter where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Setter

-- | A 'Setter' is only legal if the following laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
testSetter
  :: ( Eq s, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     , Serial IO s
     )
  => ASetter' s a -> TestTree
testSetter l = testGroup "Setter Laws"
  [ testProperty "over l id ≡ id" $ setterId l series
  , testProperty "set l y (set l x a) ≡ set l y a" $
      setterSetSet l series series series
  , testProperty "over l f . over l g ≡ over l (f . g)" $
      setterComposition l series series series
  ]
