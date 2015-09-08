# Lens SmallCheck

[![Hackage Version](https://img.shields.io/hackage/v/tasty-lens.svg)](https://hackage.haskell.org/package/tasty-lens)
[![Build Status](https://img.shields.io/travis/jdnavarro/tasty-lens.svg)](https://travis-ci.org/jdnavarro/tasty-lens)

Preassembled [`tasty`](https://hackage.haskell.org/package/tasty) `TestTree`s
for the validation:

- `Lens`
- `Setter`
- `Traversal`
- `Iso`
- `Prism`

It uses [`smallcheck-lens`](https://github.com/jdnavarro/smallcheck-laws) under
the hood. If you don't find any `TestTree`s that suit you, try the
`smallcheck-lens` package. You can use this package as a reference to implement
your own `smallcheck-lens` `TestTree`s.

Check the
[tests](https://github.com/jdnavarro/tasty-lens/blob/master/tests/tasty.hs) in
this package for examples.

## Contact

Contributions and bug reports are welcome!

Please feel free to contact jdnavarro on the #haskell IRC channel on
irc.freenode.net.
