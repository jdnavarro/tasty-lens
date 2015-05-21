{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Test.Tasty.SmallCheck.Traversal where

import Control.Lens
import Test.SmallCheck.Series (Serial(series), CoSerial(coseries), Series, localDepth)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

import Test.SmallCheck.Traversal
import Test.Tasty.SmallCheck.Setter

testTraversal
  :: forall s a. ( Eq s, Show s, Show a
                 , Serial IO a, Serial Identity a, CoSerial IO a
                 , Serial IO s
                 )
  => Traversal' s a -> TestTree
testTraversal t = testGroup "Traversal Laws"
  [ testProperty "t pure ≡ pure" $ traversePureMaybe t series
  , testProperty "fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)" $
       traverseCompose t series (coseries series' :: Series IO (a -> [a]))
                                (coseries series' :: Series IO (a -> Maybe a))
  , testSetter t
  ]
    where series' = localDepth (\d -> d - 4) series
