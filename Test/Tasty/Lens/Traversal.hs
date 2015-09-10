{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Traversal as Traversal
--
module Test.Tasty.Lens.Traversal
  ( test
  , testSeries
  , testExhaustive
  , module Test.SmallCheck.Lens.Traversal
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Data.Proxy (Proxy(..))

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial, Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import qualified Test.SmallCheck.Lens.Traversal as Traversal
import Test.SmallCheck.Lens.Traversal (compositionSum)
import qualified Test.Tasty.Lens.Setter as Setter

-- | A 'Traversal'' is only legal if it is a valid 'Setter'' (see
-- 'testSetter'), and if the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@
test
  :: forall f s a .
     ( Applicative f 
     , Eq s, Eq (f s), Eq (f (f s))
     , Show s, Show a, Show (f a)
     , Serial IO s
     , Serial Identity a, Serial IO a, Serial IO (f a), CoSerial IO a
     )
  => Proxy f -> Traversal' s a -> TestTree
test p t = testSeries p t series

-- | A 'Traversal'' is only legal if it is a valid 'Setter'' (see
-- 'testSetter'), and if the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@
testSeries
  :: forall f s a .
     ( Applicative f 
     , Eq s, Eq (f s), Eq (f (f s))
     , Show s, Show a, Show (f a)
     , Serial Identity a, Serial IO a, Serial IO (f a), CoSerial IO a
     )
  => Proxy f -> Traversal' s a -> Series IO s -> TestTree
testSeries p t ss = testGroup "Traversal Laws"
  [ testProperty "t pure ≡ pure" $ Traversal.pure p t ss
  , testProperty "fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
       compositionSum t ss (series :: Series IO (a -> f a))
                           (series :: Series IO (a -> f a))
  , Setter.testSeries t ss
  ]


-- | A 'Traversal'' is only legal if it is a valid 'Setter'' (see
-- 'testSetter'), and if the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@
testExhaustive
  :: forall f s a .
     ( Applicative f 
     , Eq s, Eq (f s), Eq (f (f s))
     , Show s, Show a, Show (f a)
     , Serial IO s
     , Serial Identity a, Serial IO a, Serial IO (f a), CoSerial IO a
     )
  => Proxy f -> Traversal' s a -> TestTree
testExhaustive p t = testGroup "Traversal Laws"
  [ testProperty "t pure ≡ pure" $ Traversal.pure p t series
  , testProperty "fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
       compositionSum t series (series :: Series IO (a -> f a))
                               (series :: Series IO (a -> f a))
  , Setter.testExhaustive t
  ]
