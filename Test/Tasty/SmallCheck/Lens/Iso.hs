{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Test.Tasty.SmallCheck.Lens.Iso where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Iso
import Test.Tasty.SmallCheck.Lens.Lens

-- | An 'Iso'' is only legal if the following laws hold:
--
-- 1. @s ^. l . from l ≡ s@
--
-- 2. @s ^. from l . l ≡ s@
--
-- An 'Iso'' is also a valid 'Lens'' in both normal and reverse form. Check
-- 'testLens'.
testIso
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     , Serial IO s, Serial Identity s, CoSerial IO s
     )
  => Iso' s a -> TestTree
testIso l = testGroup "Iso Laws"
  [ testProperty "s ^. l . from l ≡ s" $ isoHither l series
  , testProperty "s ^. from l . l ≡ s" $ isoYon l series
  , testLens l
  , testLens (from l)
  ]
