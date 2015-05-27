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
module Test.Tasty.SmallCheck.Lens
  (
  -- * Test Trees
    testIso
  , testLens
  , testPrism
  , testSetter
  , testTraversal
  ) where

import Test.Tasty.SmallCheck.Lens.Iso
import Test.Tasty.SmallCheck.Lens.Lens
import Test.Tasty.SmallCheck.Lens.Prism
import Test.Tasty.SmallCheck.Lens.Setter
import Test.Tasty.SmallCheck.Lens.Traversal
