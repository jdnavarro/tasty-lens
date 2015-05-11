{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Tasty.SmallCheck.Traversal where

import Control.Lens
import Test.SmallCheck.Series -- (Series)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Traversal

testTraversal
  :: forall s a. (Eq s, Show s, Show a, Serial IO a, Serial Identity a, CoSerial IO a)
  => Traversal' s a -> Series IO s -> TestTree
testTraversal t se = testGroup "Traversal Laws"
  [ testProperty "t pure ≡ pure" $ traversePureMaybe t se
  , testProperty "fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
       traverseCompose t se (coseries series :: Series IO (a -> [a]))
                            (coseries series :: Series IO (a -> Maybe a))
  ]
