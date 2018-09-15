# [`network`](http://hackage.haskell.org/package/network) [![Build Status](https://travis-ci.org/haskell/network.svg?branch=master)](https://travis-ci.org/haskell/network) [![Build status](https://ci.appveyor.com/api/projects/status/5erq63o4m29bhl57/branch/master?svg=true)](https://ci.appveyor.com/project/eborden/network/branch/master)

To build this package using Cabal directly from git, you must run
`autoreconf` before the usual Cabal build steps
(configure/build/install).  `autoreconf` is included in the
[GNU Autoconf](http://www.gnu.org/software/autoconf/) tools.  There is
no need to run the `configure` script: the `setup configure` step will
do this for you.

## Support Policy

### GHC

`network`'s GHC policy supports 3 [stable](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/intro.html#ghc-version-numbering-policy) versions. The current stable
version and two previous stable versions are supported.

### Hugs, JHC, UHC

`network` does not officially support these compilers.
