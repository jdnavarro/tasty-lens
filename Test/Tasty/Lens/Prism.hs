{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Prism as Prism
--
module Test.Tasty.Lens.Prism
  (
  -- * Tests
    test
  , testExhaustive
  -- * Re-exports
  , module Test.SmallCheck.Lens.Prism
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens
import Test.SmallCheck.Series (Serial(series), Series, CoSerial)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Prism (yin, yang)
import qualified Test.Tasty.Lens.Traversal as Traversal

-- | A 'Prism'' is only legal if it's a valid 'Traversal'' and if the following
--   laws hold:
--
-- 1. @preview l (review l b) ≡ Just b"@
--
-- 2. @maybe s (review l) (preview l s) ≡ s@
--
-- It uses the 'Serial' and 'CoSerial' instances for @s@ and @a@. If you are
-- not creating your own orphan instances be aware of combinatorial explosion
-- since the default implementations usually aim for exhaustivity.
--
-- This also uses "Test.Tasty.Lens.Traversal"@.@'Traversal.test', with the
-- 'Maybe' functor, to validate the 'Prism'' is a valid 'Traversal''.
test
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO s
     , Serial IO a, Serial Identity a, CoSerial IO a
     )
  => Prism' s a -> TestTree
test l = testSeries l series

-- | A 'Prism'' is only legal if it's a valid 'Traversal'' and if the following
--   laws hold:
--
-- 1. @preview l (review l b) ≡ Just b"@
--
-- 2. @maybe s (review l) (preview l s) ≡ s@
--
-- Here you explicitly pass a custom 'Series' for @s@, while for @a@ the
-- @Serial@ instance is used. If you want to fine tune both 'Series', you
-- should create your own 'TestTree'.
--
-- This also uses "Test.Tasty.Lens.Traversal"@.@'Traversal.testSeries', with
-- the 'Maybe' functor and the custom @s@ 'Series', to validate the 'Prism'' is
-- a valid 'Traversal''.
testSeries
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO a, Serial Identity a, CoSerial IO a
     )
  => Prism' s a -> Series IO s -> TestTree
testSeries l ss = testGroup "Prism Laws"
  [ testProperty "preview l (review l b) ≡ Just b" $ yin l series
  , testProperty "maybe s (review l) (preview l s) ≡ s" $ yang l ss
  , Traversal.testSeries (Proxy :: Proxy Maybe) l ss
  ]

-- | A 'Prism'' is only legal if it's a valid 'Traversal'' and if the following
--   laws hold:
--
-- 1. @preview l (review l b) ≡ Just b"@
--
-- 2. @maybe s (review l) (preview l s) ≡ s@
--
-- This is the same as 'test' except it uses
-- "Test.Tasty.Lens.Traversal"@.@'Traversal.testExhaustive' to validate
-- 'Traversal'' laws. Be aware of combinatorial explosions.
testExhaustive
  :: ( Eq s, Eq a, Show s, Show a
     , Serial IO s
     , Serial IO a, Serial Identity a, CoSerial IO a
     )
  => Prism' s a -> TestTree
testExhaustive l = testGroup "Prism Laws"
  [ testProperty "preview l (review l b) ≡ Just b" $ yin l series
  , testProperty "maybe s (review l) (preview l s) ≡ s" $ yang l series
  , Traversal.testExhaustive (Proxy :: Proxy Maybe) l
  ]
