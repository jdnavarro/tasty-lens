{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Iso as Iso
--
module Test.Tasty.Lens.Iso
  ( test
  , testSeries
  , module Test.SmallCheck.Lens.Iso
  ) where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial, Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Iso (hither, yon)
import qualified Test.Tasty.Lens.Lens as Lens

test
  :: ( Eq s, Eq a, Show s, Show a
     , Serial Identity s, Serial IO s, CoSerial IO s
     , Serial Identity a, Serial IO a, CoSerial IO a
     )
  => Iso' s a -> TestTree
test l = testSeries l series series

-- | An 'Iso'' is only legal if the following laws hold:
--
-- 1. @s ^. l . from l ≡ s@
--
-- 2. @s ^. from l . l ≡ s@
--
-- An 'Iso'' is also a valid 'Lens'' in both normal and reverse form. Check
-- 'testLens'.
testSeries
  :: ( Eq s, Eq a, Show s, Show a
     , Serial Identity s, Serial IO s, CoSerial IO s
     , Serial Identity a, Serial IO a, CoSerial IO a
     )
  => Iso' s a -> Series IO s -> Series IO a -> TestTree
testSeries l ss as = testGroup "Iso Laws"
  [ testProperty "s ^. l . from l ≡ s" $ hither l ss
  , testProperty "s ^. from l . l ≡ s" $ yon l as
  , Lens.testSeries l ss
  , Lens.testSeries (from l) as
  ]
