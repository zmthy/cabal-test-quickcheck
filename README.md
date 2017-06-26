cabal-test-quickcheck
=====================

[![Build Status](https://secure.travis-ci.org/zmthy/cabal-test-quickcheck.svg)](http://travis-ci.org/zmthy/cabal-test-quickcheck)

Support for QuickCheck with the 'detailed' Cabal testing interface. See
`test/Example.hs` for an example of use.

Set the `type` of your test-suite to `detailed-0.9`, and point the
`test-module` field to the module which exports your `tests`. Configure
with the `--enable-tests` option and run `cabal test`.
