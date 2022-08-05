module Main where

import BDD
import ATTParser
import Flags

import Control.Monad (forM_)
import System.Console.GetOpt

main :: IO ()
main = specMain $ do
  describe "asm parser" $ do
    -- 64bit
    forM_ [("x86_64 linux", "test/asm/x86_64-linux.s")
          ,("x86_64 macos", "test/asm/x86_64-mac.s")
          ,("x86_64 mingw", "test/asm/x86_64-mingw32.s")
          ,("aarch64 ios",  "test/asm/aarch64-ios.s")
          ,("aarch64 linux","test/asm/aarch64.s")
          ,("sparc64 linux","test/asm/sparc64-linux.s")
          ,("mips64 linux", "test/asm/mips64-linux.s")
          ,("powerpc64 linux","test/asm/powerpc64-linux.s")
          ,("powerpc64le linux","test/asm/powerpc64le-linux.s")
          ,("hppa linux",   "test/asm/hppa-linux.s")
          ,("m68k linux",   "test/asm/m68k-linux.s")
          ,("alpha linux",  "test/asm/alpha-linux.s")
          ,("ia64 linux",   "test/asm/ia64-linux.s")
          ,("nios2 linux",  "test/asm/nios2-linux.s")
          ,("s390 linux",   "test/asm/s390-linux.s")
          ,("s390x linux",  "test/asm/s390x-linux.s")
          ,("sh4 linux",    "test/asm/sh4-linux.s")
          ]
      $ \(d, f) ->do
      describe d $ do
        x <- runIO $ parse f

        it "x should be 1" $ do
          lookupInteger "x" x `shouldBe` (Just 1)
        it "z should be 0xffffffffffffffff" $ do
          lookupInteger "y" x `shouldBe` (Just 0xffffffffffffffff)
        it "z should be -1" $ do
          lookupInteger "z" x `shouldBe` (Just (-1))

        it "t should be \"Hello World\\\"\\n\\0\"" $ do
          lookupString "t" x `shouldBe` (Just "Hello World\" 12345\0")
    -- 32 bit
    forM_ [("arm ios",      "test/asm/arm-ios.s")
          ,("arm linux",    "test/asm/arm.s")
          ,("x86 linux",    "test/asm/x86-linux.s")
          ,("sparc linux",  "test/asm/sparc-linux.s")
          ,("mips linux",   "test/asm/mips-linux.s")
          ,("powerpc linux","test/asm/powerpc-linux.s")
          ]
      $ \(d, f) ->do
      describe d $ do
        x <- runIO $ parse f

        it "x should be 1" $ do
          lookupInteger "x" x `shouldBe` (Just 1)
        it "z should be 0xffffffff" $ do
          lookupInteger "y" x `shouldBe` (Just 0xffffffff)
        it "z should be -1" $ do
          lookupInteger "z" x `shouldBe` (Just (-1))

        it "t should be \"Hello World\\\"\\n\\0\"" $ do
          lookupString "t" x `shouldBe` (Just "Hello World\" 12345\0")

  describe "flags" $ do
    it "are processed in order" $ do
      -- at the moment this test fails (issue #35)
      let (fs, files, errs) = getOpt Permute options
              [ "--cc=gcc", "--cc=clang"
              , "--include=<include1.h>", "--include=<include2.h>"
              , "--template", "template1", "--template=template2"
              ]
      let mode = foldl (\m f -> f m) emptyMode fs

      configModeMaybe mode cmCompiler `shouldBe` Just "clang"
      configModeMaybe mode cmTemplate `shouldBe` Just "template2"
      configMode      mode cFlags     `shouldBe` Just [Include "<include1.h>", Include "<include2.h>"]

      files `shouldBe` []
      errs `shouldBe` []

configMode :: Mode -> (ConfigM Maybe -> a) -> Maybe a
configMode (UseConfig c) f = Just (f c)
configMode _             _ = Nothing

configModeMaybe :: Mode -> (ConfigM Maybe -> Maybe a) -> Maybe a
configModeMaybe (UseConfig c) f = f c
configModeMaybe _             _ = Nothing
