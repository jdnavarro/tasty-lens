{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.Lens.Traversal where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial, Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Lens.Traversal
import Test.Tasty.SmallCheck.Lens.Setter

-- | A 'Traversal'' is only legal if it is a valid 'Setter'' (see
-- 'testSetter'), and if the following laws hold:
--
-- 1. @t pure ≡ pure@
--
-- 2. @fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)@
testTraversal
  :: forall s a. ( Eq s, Show s, Show a
                 , Serial IO a, Serial Identity a, CoSerial IO a
                 , Serial IO s
                 )
  => Traversal' s a -> TestTree
testTraversal t = testGroup "Traversal Laws"
  [ testProperty "t pure ≡ pure" $ traversePureMaybe t series
  , testProperty "fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
       traverseCompose t series (series :: Series IO (a -> [a]))
                                (series :: Series IO (a -> Maybe a))
  , testSetter t
  ]
