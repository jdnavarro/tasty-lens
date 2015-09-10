{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is intended to be imported @qualified@, for example:
--
-- > import qualified Test.Tasty.Lens.Traversal as Traversal
--
module Test.Tasty.Lens.Traversal where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial, Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Traversal (compositionSum)
import qualified Test.Tasty.Lens.Setter as Setter

-- | A 'Traversal'' is only legal if it is a valid 'Setter'' (see
-- 'testSetter'), and if the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@
test
  :: forall s a. ( Eq s, Show s, Show a
                 , Serial IO a, Serial Identity a, CoSerial IO a
                 , Serial IO s
                 )
  => Traversal' s a -> TestTree
test t = testGroup "Traversal Laws"
  [ -- XXX: testProperty "t pure ≡ pure" $ Traversal.pure t series
    testProperty "fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
       compositionSum t series (series :: Series IO (a -> [a]))
                               (series :: Series IO (a -> Maybe a))
  , Setter.test t
  ]
