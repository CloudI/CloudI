## 0.68.8

 - Process flags in order, now the last of `--compiler`, `--linker`,
    `--template` is used. (#35)

 - WINIO: Make sure that with we don't use the TEMP workaround (#52)

## 0.68.7

 - The C compiler is now assumed to be called `cc` instead of `gcc`
   by default (#42)

 - Fix race condition when using response files (#30)

 - Add extra diagnostics when `hsc2hs` sub-process fails
   and make TempFile creation fully atomic on Windows. See (#33)

## 0.68.6

 - Supports generation of response files to avoid system filepath
   limits (#22, #23)

 - Fix non-deterministic failures for response file handlings (#29)

 - Temporary file removals on Windows are not a bit more reliable and should
   throw less access denied errors.  See #25 and
   ([#9775](https://gitlab.haskell.org/ghc/ghc/issues/9775))

 - Accept a leading single quote for data constructor promotion (#13, #17)

 - Support `MonadFail` / base-4.13

 - Include template file as first header in hsc2hs generated C file (#28)

 - On Windows define `__USE_MINGW_ANSI_STDIO` to 1 instead of 0 when not already
   defined in standard template header.  This is a more modern default (#28)

## 0.68.5

 - Support response files regardless of which GHC `hsc2hs` was compiled
   with ([#15758](https://ghc.haskell.org/trac/ghc/ticket/15758))

 - Support for non-x86 platforms should be significantly more robust due to
   improvements in `hsc2hs`'s assembly parser

 - Add support for haskell files that use a leading single quote for promoted
   data constructors.

## 0.68.4

 - Add support to read command line arguments supplied via response files
   ([#13896](https://ghc.haskell.org/trac/ghc/ticket/13388))

## 0.68.2

 - Support GHC 8.2.1

 - Make `hsc_alignment` macro work in clang
   ([D3346](https://phabricator.haskell.org/D3346))

 - Track column numbers to improve GHC's caret diagnostic display
   ([#13388](https://ghc.haskell.org/trac/ghc/ticket/13388))

## 0.68.1

 - Fix type signature of generated `main` test function
   to avoid C compiler warnings about unused `argc`/`argv`
   function parameters during feature testing.

 - Double-escape paths used to build call to `hsc_line`
   ([#12504](http://ghc.haskell.org/ticket/12504))
