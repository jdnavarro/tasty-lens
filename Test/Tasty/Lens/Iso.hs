{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Iso as Iso
--
module Test.Tasty.Lens.Iso
  (
  -- * Tests
    test
  , testSeries
  , testExhaustive
  ) where

import Control.Lens
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.DumbCheck -- (Serial(series), CoSerial, Series)

import Control.Lens.Iso.Laws (hither, yon)
import qualified Test.Tasty.Lens.Lens as Lens

-- | An 'Iso'' is only legal if the following laws hold:
--
-- 1. @s ^. l . from l ≡ s@
--
-- 2. @s ^. from l . l ≡ s@
--
-- It uses the 'Serial' and 'CoSerial' instances for @s@ and @a@. If you are
-- not creating your own orphan instances be aware of combinatorial explosion
-- since the default implementations usually aim for exhaustivity.
--
-- This uses "Test.Tasty.Lens.Lens"@.@'Lens.test' to validate the 'Iso'' is
-- a valid 'Lens'' in both normal and reverse form.
test
  :: ( Eq s, Eq a, Show s, Show a, Serial s, Serial a)
  => Iso' s a -> TestTree
test = testExhaustive

-- | An 'Iso'' is only legal if the following laws hold:
--
-- 1. @s ^. l . from l ≡ s@
--
-- 2. @s ^. from l . l ≡ s@
--
-- As in 'test' both @s@ and @a@ need to be 'Serial' and 'CoSerial' instances,
-- but here you explicitly pass custom 'Series' of @s@ and @a@ for the forward
-- case, while the reverse case still uses 'Serial' and 'CoSerial' instances.
-- If you want to fine tune both the forward and reverse cases, you should
-- create your own 'TestTree'.
--
-- This uses "Test.Tasty.Lens.Lens"@.@'Lens.testSeries', with the custom
-- 'Series' for @s@ and @a@, to validate the 'Iso'' is a valid 'Lens'' in both
-- normal and reverse form.
testSeries
  :: (Eq s, Eq a, Show s, Show a, Serial s, Serial a)
  => Iso' s a -> Series s -> Series a -> TestTree
testSeries l ss as = testGroup "Iso Laws"
  [ testSeriesProperty "s ^. l . from l ≡ s" (hither l) ss
  , testSeriesProperty "s ^. from l . l ≡ s" (yon l) as
  , Lens.testSeries l ss
  , Lens.testSeries (from l) as
  ]

-- | An 'Iso'' is only legal if the following laws hold:
--
-- 1. @s ^. l . from l ≡ s@
--
-- 2. @s ^. from l . l ≡ s@
--
-- This is the same as 'test' except it uses
-- "Test.Tasty.Lens.Lens"@.@'Lens.testExhaustive' to validate the 'Lens'' laws.
-- Be aware of combinatorial explosions.
testExhaustive
  :: (Eq s, Eq a, Show s, Show a, Serial s, Serial a)
  => Iso' s a -> TestTree
testExhaustive l = testGroup "Iso Laws"
  [ testSerialProperty "s ^. l . from l ≡ s" (hither l)
  , testSerialProperty "s ^. from l . l ≡ s" (yon l)
  , Lens.testExhaustive l
  , Lens.testExhaustive (from l)
  ]
