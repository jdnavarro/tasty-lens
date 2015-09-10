{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Lens as Lens
--
module Test.Tasty.Lens.Lens
  ( test
  , module Test.SmallCheck.Lens.Lens
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Lens (setView, viewSet)
import qualified Test.Tasty.Lens.Traversal as Traversal

-- | A 'Lens'' is only legal if it is a valid 'Traversal'' (see
--   'testTraversal'), and if the following laws hold:
--
-- 1. @view l (set l b a)  ≡ b@
--
-- 2. @set l (view l a) a  ≡ a@
--
-- 3. @set l c (set l b a) ≡ set l c a@
test
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     , Serial IO s
     )
  => Lens' s a -> TestTree
test l = testGroup "Lens Laws"
  [ testProperty "view l (set l b a) ≡ b" $
      setView l series
  , testProperty "set l (view l a) a ≡ a" $
      viewSet l series series
  , Traversal.test (Proxy :: Proxy Maybe) l
  ]
