{-# LANGUAGE RankNTypes #-}
module Test.SmallCheck.Iso where

import Control.Lens
import Test.SmallCheck (Property)
import qualified Test.SmallCheck as SC (over)
import Test.SmallCheck.Series (Series, Serial)

isoHither
  :: (Eq s, Show s, Eq a, Show a, Serial m a)
  => AnIso' s a -> Series m s -> Property m
isoHither l ss = SC.over ss $ \s ->
    s ^. cloneIso l . from l == s

isoYon
  :: (Eq s, Show s, Eq a, Show a, Serial m a)
  => AnIso' s a -> Series m a -> Property m
isoYon l as = SC.over as $ \a -> a ^. from l . cloneIso l == a
