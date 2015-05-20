{-# LANGUAGE RankNTypes #-}
module Test.SmallCheck.Prism where

import Control.Lens
import Test.SmallCheck (Property)
import qualified Test.SmallCheck as SC (over)
import Test.SmallCheck.Series (Series, Serial)

prismYin
  :: (Eq s, Show s, Eq a, Show a, Serial m a)
  => Prism' s a -> Series m a -> Property m
prismYin l as = SC.over as $ \a -> preview l (review l a) == Just a

prismYang
  :: (Eq s, Show s, Eq a, Show a, Serial m a)
  => Prism' s a -> Series m s -> Property m
prismYang l ss = SC.over ss $ \s -> maybe s (review l) (preview l s) == s
