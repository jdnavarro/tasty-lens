# Lens SmallCheck

[![Hackage Version](https://img.shields.io/hackage/v/smallcheck-lens.svg)](https://hackage.haskell.org/package/smallcheck-lens) [![Build Status](https://img.shields.io/travis/jdnavarro/smallcheck-lens.svg)](https://travis-ci.org/jdnavarro/smallcheck-lens)

[`smallcheck`](https://hackage.haskell.org/package/smallcheck) properties
ported from
[`lens-properties`](https://hackage.haskell.org/package/lens-properties)
and [`tasty`](https://hackage.haskell.org/package/tasty tasty) test trees
to validate `Lens`es, `Setter`s, `Traversal`s, `Iso`s and `Prism`s.

Most likely, you will only need the `Test.Tasty.SmallCheck.Lens` module,
which includes test trees ready to be run.

Check the
[tests](https://github.com/jdnavarro/smallcheck-lens/blob/master/tests/tasty.hs)
in this package for examples.
