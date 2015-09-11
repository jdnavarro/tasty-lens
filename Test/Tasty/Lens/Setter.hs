{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Setter as Setter
--
module Test.Tasty.Lens.Setter
  (
  -- * Tests
    test
  , testSeries
  , testExhaustive
  -- * Re-exports
  , module Test.SmallCheck.Lens.Setter
  ) where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Setter
  ( identity
  , setSet
  , setSetSum
  , composition
  , compositionSum
  )

-- | A 'Setter' is only legal if the following laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
--
-- The 'Serial' and 'CoSerial' instances for @s@ and @a@. If you are
-- not creating your own orphan instances be aware of combinatorial explosion
-- since the default implementations usually aim for exhaustivity.
--
-- In this case @f@ and @g@ are of type @a -> a@ and when combining them the
-- /sum/ of 'Series' is used.
test
  :: ( Eq s, Show s, Show a
     , Serial IO s
     , Serial IO a, Serial Identity a, Serial IO (a -> a)
     )
  => Setter' s a -> TestTree
test l = testSeries l series

-- | A 'Setter' is only legal if the following laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
--
-- Here you explicitly pass a custom 'Series' for @s@, while for @a@ the
-- @Serial@ instance is used. If you want to fine tune both 'Series', you
-- should create your own 'TestTree'.
--
-- In this case @f@ and @g@ are of type @a -> a@ and when combining them the
-- /sum/ of 'Series' is used.
testSeries
  :: ( Eq s, Show s, Show a
     , Serial IO a, Serial Identity a, Serial IO (a -> a)
     )
  => Setter' s a -> Series IO s -> TestTree
testSeries l ss = testGroup "Setter Laws"
  [ testProperty "over l id ≡ id" $ identity l ss
  , testProperty "set l y (set l x a) ≡ set l y a" $
      setSetSum l ss series series
  , testProperty "over l f . over l g ≡ over l (f . g)" $
      compositionSum l ss series series
  ]

-- | A 'Setter' is only legal if the following laws hold:
--
-- 1. @set l y (set l x a) ≡ set l y a@
--
-- 2. @over l id ≡ id@
--
-- 3. @over l f . over l g ≡ over l (f . g)@
--
-- This is the same as 'test' except it uses the /product/ when combining the
-- @f@ and @g@ 'Series'.
testExhaustive
  :: ( Eq s, Show s, Show a
     , Serial IO s
     , Serial IO a, Serial Identity a, Serial IO (a -> a)
     )
  => Setter' s a -> TestTree
testExhaustive l = testGroup "Setter Laws"
  [ testProperty "over l id ≡ id" $ identity l series
  , testProperty "set l y (set l x a) ≡ set l y a" $
      setSet l series series series
  , testProperty "over l f . over l g ≡ over l (f . g)" $
      composition l series series series
  ]
