{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Test.Tasty.SmallCheck.Lens.Prism where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Prism
import Test.Tasty.SmallCheck.Lens.Traversal

-- | A 'Prism'' is only legal if it is a valid 'Traversal'' (see
--   'testTraversal'), and if the following laws hold:
--
-- 1. @preview l (review l b) ≡ Just b"@
--
-- 2. @maybe s (review l) (preview l s) ≡ s@
testPrism
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     , Serial IO s
     )
  => Prism' s a -> TestTree
testPrism l = testGroup "Prism Laws"
  [ testTraversal l
  , testProperty "preview l (review l b) ≡ Just b" $
      prismYin l series
  , testProperty "maybe s (review l) (preview l s) ≡ s" $
      prismYang l series
  ]
