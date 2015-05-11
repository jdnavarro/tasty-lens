{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.SmallCheck.Traversal where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, pure)
#endif
import Control.Lens
import Data.Functor.Compose (Compose(..), getCompose)
import Test.SmallCheck (Property)
import qualified Test.SmallCheck as SC (over)
import Test.SmallCheck.Series (Serial, Series)

traversePure
  :: forall m f s a. (Monad m, Show s, Applicative f, Eq (f s))
  => LensLike' f s a -> Series m s -> Property m
traversePure l ss = SC.over ss $ \s -> l pure s == (pure s :: f s)

traversePureMaybe
  :: (Monad m, Show s, Eq s)
  => LensLike' Maybe s a -> Series m s -> Property m
traversePureMaybe = traversePure

traverseCompose
  :: ( Monad m, Show s, Show a, Show (f a), Show (g a)
     , Applicative f, Applicative g, Eq (g (f s)), Serial Identity a
     )
  => Traversal' s a -> Series m s -> Series m (a -> f a) -> Series m (a -> g a) -> Property m
traverseCompose t ss fs gs = SC.over ss $ \s -> SC.over fs $ \f -> SC.over gs $ \g ->
    (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s
