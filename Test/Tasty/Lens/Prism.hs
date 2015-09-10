{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Prism as Prism
--
module Test.Tasty.Lens.Prism where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Prism
import qualified Test.Tasty.Lens.Traversal as Traversal

-- | A 'Prism'' is only legal if it is a valid 'Traversal'' (see
--   'testTraversal'), and if the following laws hold:
--
-- 1. @preview l (review l b) ≡ Just b"@
--
-- 2. @maybe s (review l) (preview l s) ≡ s@
test
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     , Serial IO s
     )
  => Prism' s a -> TestTree
test l = testGroup "Prism Laws"
  [ Traversal.test l
  , testProperty "preview l (review l b) ≡ Just b" $
      prismYin l series
  , testProperty "maybe s (review l) (preview l s) ≡ s" $
      prismYang l series
  ]
