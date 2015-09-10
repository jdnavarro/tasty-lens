{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Prism as Prism
--
module Test.Tasty.Lens.Prism
  ( test
  , module Test.SmallCheck.Lens.Prism
  ) where

import Data.Proxy (Proxy (..))
import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Prism (yin, yang)
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
  [ Traversal.test (Proxy :: Proxy Maybe) l
  , testProperty "preview l (review l b) ≡ Just b" $ yin l series
  , testProperty "maybe s (review l) (preview l s) ≡ s" $ yang l series
  ]
