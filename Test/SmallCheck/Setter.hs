{-# LANGUAGE FlexibleContexts #-}
module Test.SmallCheck.Setter where

import Control.Lens
import Test.SmallCheck (Property)
import qualified Test.SmallCheck as SC (over)
import Test.SmallCheck.Series (Series, Serial)

setterId
  :: (Eq s, Monad m, Show s)
  => ASetter' s a -> Series m s -> Property m
setterId l se = SC.over se $ \s -> over l id s == s

setterComposition
  :: (Monad m, Eq s, Show s, Show a, Serial Identity a)
  => ASetter' s a
  -> Series m s
  -> Series m (a -> a)
  -> Series m (a -> a)
  -> Property m
setterComposition l ss fs gs =
    SC.over ss $ \s ->
        SC.over fs $ \f ->
            SC.over gs $ \g ->
    over l f (over l g s) == over l (f . g) s

setterSetSet
  :: (Monad m, Eq s, Show s, Show a)
  => ASetter' s a -> Series m s -> Series m a -> Series m a -> Property m
setterSetSet l ss as bs =
    SC.over ss $ \s ->
        SC.over as $ \a ->
            SC.over bs $ \b ->
    set l b (set l a s) == set l b s
