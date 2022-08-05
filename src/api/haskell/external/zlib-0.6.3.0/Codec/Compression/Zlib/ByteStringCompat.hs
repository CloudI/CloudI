-- Lifted from the text package, with light editing:
-- Data.Text.Internal.ByteStringCompat

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Codec.Compression.Zlib.ByteStringCompat (mkBS, withBS) where

import Data.ByteString.Internal (ByteString (..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

#if MIN_VERSION_base(4,10,0)
import GHC.ForeignPtr (plusForeignPtr)
#else
import GHC.ForeignPtr (ForeignPtr(ForeignPtr))
import GHC.Types (Int (..))
import GHC.Prim (plusAddr#)
#endif

mkBS :: ForeignPtr Word8 -> Int -> Int -> ByteString
#if MIN_VERSION_bytestring(0,11,0)
mkBS dfp o n = BS (plusForeignPtr dfp o) n
#else
mkBS dfp o n = PS dfp o n
#endif
{-# INLINE mkBS #-}

withBS :: ByteString -> (ForeignPtr Word8 -> Int -> r) -> r
#if MIN_VERSION_bytestring(0,11,0)
withBS (BS !sfp !slen)       kont = kont sfp slen
#else
withBS (PS !sfp !soff !slen) kont = kont (plusForeignPtr sfp soff) slen
#endif
{-# INLINE withBS #-}

#if !MIN_VERSION_base(4,10,0)
-- |Advances the given address by the given offset in bytes.
--
-- The new 'ForeignPtr' shares the finalizer of the original,
-- equivalent from a finalization standpoint to just creating another
-- reference to the original. That is, the finalizer will not be
-- called before the new 'ForeignPtr' is unreachable, nor will it be
-- called an additional time due to this call, and the finalizer will
-- be called with the same address that it would have had this call
-- not happened, *not* the new address.
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr guts) (I# offset) = ForeignPtr (plusAddr# addr offset) guts
{-# INLINE [0] plusForeignPtr #-}
{-# RULES
"ByteString plusForeignPtr/0" forall fp .
   plusForeignPtr fp 0 = fp
 #-}
#endif
