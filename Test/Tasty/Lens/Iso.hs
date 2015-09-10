{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Iso as Iso
--
module Test.Tasty.Lens.Iso where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Iso
import qualified Test.Tasty.Lens.Lens as Lens

-- | An 'Iso'' is only legal if the following laws hold:
--
-- 1. @s ^. l . from l ≡ s@
--
-- 2. @s ^. from l . l ≡ s@
--
-- An 'Iso'' is also a valid 'Lens'' in both normal and reverse form. Check
-- 'testLens'.
test
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     , Serial IO s, Serial Identity s, CoSerial IO s
     )
  => Iso' s a -> TestTree
test l = testGroup "Iso Laws"
  [ testProperty "s ^. l . from l ≡ s" $ isoHither l series
  , testProperty "s ^. from l . l ≡ s" $ isoYon l series
  , Lens.test l
  , Lens.test (from l)
  ]
