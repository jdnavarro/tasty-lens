{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Test.Tasty.SmallCheck.Lens.Lens where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Lens
import Test.Tasty.SmallCheck.Lens.Traversal

-- | A 'Lens'' is only legal if it is a valid 'Traversal'' (see
--   'testTraversal'), and if the following laws hold:
--
-- 1. @view l (set l b a)  ≡ b@
--
-- 2. @set l (view l a) a  ≡ a@
--
-- 3. @set l c (set l b a) ≡ set l c a@
testLens
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     , Serial IO s
     )
  => Lens' s a -> TestTree
testLens l = testGroup "Lens Laws"
  [ testProperty "view l (set l b a) ≡ b" $
      lensSetView l series
  , testProperty "set l (view l a) a ≡ a" $
      lensViewSet l series series
  , testTraversal l
  ]
