{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Lens as Lens
--
module Test.Tasty.Lens.Lens
  (
  -- * Tests
    test
  , testSeries
  , testExhaustive
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.DumbCheck -- (testProperty)

import Control.Lens.Lens.Laws (setView, viewSet)
import qualified Test.Tasty.Lens.Traversal as Traversal

-- | A 'Lens'' is only legal if it's a valid 'Traversal'' and if the following
--   laws hold:
--
-- 1. @view l (set l b a)  ≡ b@
--
-- 2. @set l (view l a) a  ≡ a@
--
-- 3. @set l c (set l b a) ≡ set l c a@
--
-- It uses the 'Serial' and 'CoSerial' instances for @s@ and @a@. If you are
-- not creating your own orphan instances be aware of combinatorial explosion
-- since the default implementations usually aim for exhaustivity.
--
-- This also uses "Test.Tasty.Lens.Traversal"@.@'Traversal.test', with the
-- 'Maybe' functor, to validate the 'Lens'' is a valid 'Traversal''.
test
  :: (Eq s, Eq a, Show s, Show a, Serial s, Serial a)
  => Lens' s a -> TestTree
test l = testSeries l series

-- | A 'Lens'' is only legal if it's a valid 'Traversal'' and if the following
--   laws hold:
--
-- 1. @view l (set l b a)  ≡ b@
--
-- 2. @set l (view l a) a  ≡ a@
--
-- 3. @set l c (set l b a) ≡ set l c a@
--
-- Here you explicitly pass a custom 'Series' for @s@, while for @a@ the
-- @Serial@ instance is used. If you want to fine tune both 'Series', you
-- should create your own 'TestTree'.
--
-- This also uses "Test.Tasty.Lens.Traversal"@.@'Traversal.testSeries', with
-- the 'Maybe' functor and the custom @s@ 'Series', to validate the 'Lens'' is
-- a valid 'Traversal''.
testSeries
  :: (Eq s, Eq a, Show s, Show a, Serial a)
  => Lens' s a -> Series s -> TestTree
testSeries l ss = testGroup "Lens Laws"
  [ testSeriesProperty "view l (set l b a) ≡ b" (setView l) ss
  , testSeriesProperty
      "set l (view l a) a ≡ a"
      (uncurry $ viewSet l)
      (zip ss series)
  , Traversal.testSeries (Proxy :: Proxy Maybe) l ss
  ]

-- | A 'Lens'' is only legal if it's a valid 'Traversal'' and if the following
-- laws hold:
--
-- 1. @view l (set l b a)  ≡ b@
--
-- 2. @set l (view l a) a  ≡ a@
--
-- 3. @set l c (set l b a) ≡ set l c a@
--
-- This is like the same as 'test' except it uses
-- "Test.Tasty.Lens.Traversal"@.@'Traversal.testExhaustive' to validate
-- 'Traversal'' laws. Be aware of combinatorial explosions.
testExhaustive
  :: (Eq s, Eq a, Show s, Show a, Serial  s, Serial a)
  => Lens' s a -> TestTree
testExhaustive l = testGroup "Lens Laws"
  [ testSerialProperty "view l (set l b a) ≡ b" (setView l)
  , testSerialProperty "set l (view l a) a ≡ a" (uncurry $ viewSet l)
  , Traversal.testExhaustive (Proxy :: Proxy Maybe) l
  ]
