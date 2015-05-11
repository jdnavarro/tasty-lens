{-# LANGUAGE RankNTypes #-}
module Test.SmallCheck.Lens where

import Control.Lens
import Test.SmallCheck (Property)
import qualified Test.SmallCheck as SC (over)
import Test.SmallCheck.Series (Series)

lensSetView :: (Monad m, Eq s, Show s) => Lens' s a -> Series m s -> Property m
lensSetView l ss = SC.over ss $ \s -> set l (view l s) s == s

lensViewSet
  :: (Monad m, Eq s, Eq a, Show s, Show a)
  => Lens' s a -> Series m s -> Series m a -> Property m
lensViewSet l ss as = SC.over ss $ \s -> SC.over as $ \a ->
    view l (set l a s) == a
