import Test.DocTest

main :: IO ()
main = doctest [
    "-i"
  , "-idist/build"
  , "-i."
  , "-idist/build/autogen"
  , "-Idist/build/autogen"
  , "-Idist/build"
  , "-Iinclude"
  , "-optP-include"
  , "-optPdist/build/autogen/cabal_macros.h"
  , "-DCALLCONV=ccall"
  , "-XCPP"
  , "-XDeriveDataTypeable"
  , "-package-db dist/package.conf.inplace"
  , "-package network"
  , "Network"
  ]
