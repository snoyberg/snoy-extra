{-# LANGUAGE MagicHash #-}
-- | See: http://stackoverflow.com/questions/27204425/add-with-carry-on-word8
module Data.Word8Carry where

import GHC.Exts (isTrue#)
import GHC.Prim (gtWord#, narrow8Word#, plusWord#)
import GHC.Word (Word, Word8 (W8#))

boxed :: Word8 -> Word8 -> (Word8, Bool)
boxed x y =
    (z, z < x)
  where
    z = x + y

primops :: Word8 -> Word8 -> (Word8, Bool)
primops (W8# x#) (W8# y#) =
    (W8# (narrow8Word# z#), isTrue# (gtWord# z# 255##))
  where
    z# = plusWord# x# y#
