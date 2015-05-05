{-# LANGUAGE FlexibleContexts #-}
module Test.SmallCheck.Setter where

import Control.Lens
import Test.SmallCheck (Property)
import qualified Test.SmallCheck as SC (over)
import Test.SmallCheck.Series (Series, Serial, CoSerial)

setterId
  :: (Eq s, Monad m, Show s)
  => ASetter' s a -> Series m s -> Property m
setterId l se = SC.over se $ \s -> over l id s == s

setterComposition
  :: (Eq s, Show s, Show a, Serial m a, Serial Identity a, CoSerial m a)
  => ASetter' s a -> Series m s -> Property m
setterComposition l se = SC.over se $ \s f g ->
    over l f (over l g s) == over l (f . g) s

setterSetSet
  :: (Eq s, Show s, Show a, Serial m a)
  => ASetter' s a -> Series m s -> Property m
setterSetSet l se = SC.over se $ \s a b ->
    set l b (set l a s) == set l b s
