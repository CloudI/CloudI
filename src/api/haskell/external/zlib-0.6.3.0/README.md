# zlib [![CI][CI badge]][CI page] [![Hackage][Hackage badge]][Hackage page]

**Compression and decompression in the gzip and zlib format**

This package provides a pure interface for compressing and decompressing streams of data represented as lazy `ByteString`s.
It uses the `zlib` C library so it has high performance. It supports the `zlib`, `gzip` and `raw` compression formats.

It provides a convenient high level API suitable for most tasks and for the few cases where more control is needed it provides access to the full zlib feature set.

[CI badge]: https://github.com/haskell/zlib/actions/workflows/haskell-ci.yml/badge.svg
[CI page]: https://github.com/haskell/zlib/actions/workflows/haskell-ci.yml
[Hackage page]: https://hackage.haskell.org/package/zlib
[Hackage badge]: https://img.shields.io/hackage/v/zlib.svg
