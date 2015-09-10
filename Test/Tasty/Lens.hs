{-|
Preassembled tasty test trees using 'Test.SmallCheck.Serial' instances and
one-to-one zipping of elements in 'Test.SmallCheck.Series' (see
'Test.SmallCheck.Series.Utils.zipLogic') when 'Test.SmallCheck.Serial'
functions are needed.

If you need more exhaustive testing coverage or if you experience combinatorial
explosion you can either override the default 'Test.SmallCheck.Serial'
instances with custom ones or you can create your own tasty test trees using
the functions at "Test.SmallCheck.Lens".
-}
module Test.Tasty.Lens
  (
  -- * Test Trees
    testIso
  , testLens
  , testPrism
  , testSetter
  , testTraversal
  ) where

import Test.Tasty.Lens.Iso
import Test.Tasty.Lens.Lens
import Test.Tasty.Lens.Prism
import Test.Tasty.Lens.Setter
import Test.Tasty.Lens.Traversal
