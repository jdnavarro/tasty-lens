# Tasty Lens

[![Hackage Version](https://img.shields.io/hackage/v/tasty-lens.svg)](https://hackage.haskell.org/package/tasty-lens)
[![Build Status](https://img.shields.io/travis/jdnavarro/tasty-lens.svg)](https://travis-ci.org/jdnavarro/tasty-lens)

Preassembled [`tasty`](https://hackage.haskell.org/package/tasty) `TestTree`s
with sensible defaults for the validation of:

- `Lens`
- `Setter`
- `Traversal`
- `Iso`
- `Prism`

Although these tests offer weaker guarantees compared to manually proving the
laws through equational reasoning, they can still be useful for regression
testing where you don't want to manually proof every time you make a slight
change that could affect the laws.

It uses [`smallcheck-lens`](https://github.com/jdnavarro/smallcheck-laws)
under the hood. If you don't find any functions to create the `TestTree`s you
are looking for, you may want to use the `smallcheck-lens` package directly.
In such case you can still check this package as a reference to implement your
own `smallcheck-lens` `TestTree`s.

Check the
[tests](https://github.com/jdnavarro/tasty-lens/blob/master/tests/tasty.hs) in
this package for usage examples.

## Contact

Contributions and bug reports are welcome!

Please feel free to contact jdnavarro on the #haskell IRC channel on
irc.freenode.net.
