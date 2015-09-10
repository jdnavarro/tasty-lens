{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Setter as Setter
--
module Test.Tasty.Lens.Setter
  ( test
  , testSeries
  , module Test.SmallCheck.Lens.Setter
  ) where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Setter (identity, setSetSum, compositionSum)

-- | A 'Setter' is only legal if the following laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
test
  :: ( Eq s, Show s, Show a
     , Serial IO s
     , Serial IO a, Serial Identity a, Serial IO (a -> a)
     )
  => Setter' s a -> TestTree
test l = testSeries l series

-- | A 'Setter' is only legal if the following laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
testSeries
  :: ( Eq s, Show s, Show a
     , Serial IO a, Serial Identity a, Serial IO (a -> a)
     )
  => Setter' s a -> Series IO s -> TestTree
testSeries l ss = testGroup "Setter Laws"
  [ testProperty "over l id ≡ id" $ identity l ss
  , testProperty "set l y (set l x a) ≡ set l y a" $
      setSetSum l ss series series
  , testProperty "over l f . over l g ≡ over l (f . g)" $
      compositionSum l ss series series
  ]
