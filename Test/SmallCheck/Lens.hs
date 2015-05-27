{-|
'Test.SmallCheck.Property' creators for every individual /Lens law/ using
explicit 'Test.SmallCheck.Series'.

Use this module when you need different 'Test.SmallCheck.Series' to the ones
implicitly in "Test.Tasty.SmallCheck.Lens" or when you don't want to use
@tasty@ as the test runner.
-}
module Test.SmallCheck.Lens
  (
  -- * Setter
    setterId
  , setterSetSet
  , setterComposition
  -- * Traversal
  , traversePure
  , traversePureMaybe
  , traverseCompose
  -- * Lens
  , lensSetView
  , lensViewSet
  -- * Prism
  , prismYin
  , prismYang
  -- * Iso
  , isoHither
  , isoYon
  ) where

import Test.SmallCheck.Lens.Iso
import Test.SmallCheck.Lens.Lens
import Test.SmallCheck.Lens.Prism
import Test.SmallCheck.Lens.Setter
import Test.SmallCheck.Lens.Traversal
