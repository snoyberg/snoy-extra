{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Extend a Reader environment providing a MWC 'Gen' into a MonadRandom.
module Control.Monad.MWC
    ( module System.Random.MWC
    , uniformM
    , uniformRM
    , runMWCReaderT
      -- * ASCII
    , uniformAsciiByte
    , uniformAsciiByteString
    , uniformAsciiText

    , uniformAsciiByteStringSimple
    , uniformAsciiTextSimple

    , uniformAsciiByteStringComplex64
    , uniformAsciiTextComplex64
    ) where

import System.Random.MWC
import Control.Monad.Reader (MonadReader, ask, ReaderT (ReaderT))
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Word (Word8, Word64)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (liftM)
import Data.Word8 (_A, _a, _0, _hyphen, _underscore)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Storable as VS
import Data.Bits (shiftR, (.&.))
import Data.ByteVector (fromByteVector)

-- | Must be instantiated by your environment type.
class HasMWC a s | a -> s where
    getMWC :: a -> Gen s
instance HasMWC (Gen s) s where
    getMWC = id

uniformM :: ( Variate a
            , PrimMonad base
            , MonadBase base m
            , MonadReader env m
            , HasMWC env (PrimState base)
            )
         => m a
uniformM = do
    env <- ask
    let gen = getMWC env
    liftBase $ uniform gen
{-# INLINE uniformM #-}

uniformRM :: ( Variate a
             , PrimMonad base
             , MonadBase base m
             , MonadReader env m
             , HasMWC env (PrimState base)
             )
          => (a, a)
          -> m a
uniformRM bounds = do
    env <- ask
    let gen = getMWC env
    liftBase $ uniformR bounds gen
{-# INLINE uniformRM #-}

-- | Convenience function for the common case of getting entropy from the
-- system and running a computation.
runMWCReaderT :: MonadBase IO m => ReaderT GenIO m a -> m a
runMWCReaderT (ReaderT f) = liftBase createSystemRandom >>= f

-- | Generate a pair of random viewable ASCII byte, using A-Z, a-z, 0-9, -, and
-- _.
uniformAsciiByte
    :: ( PrimMonad base
       , MonadBase base m
       , MonadReader env m
       , HasMWC env (PrimState base)
       ) => m Word8
uniformAsciiByte = liftM (w2ascii . high) uniformM
{-# INLINE uniformAsciiByte #-}

-- | Generate a random ByteString consisting of random ASCII bytes, following
-- @uniformAsciiByte@.
uniformAsciiByteString
    :: ( PrimMonad base
       , MonadBase base m
       , MonadReader env m
       , HasMWC env (PrimState base)
       )
    => Int -- ^ length
    -> m ByteString
uniformAsciiByteString = uniformAsciiByteStringSimple
{-# INLINE uniformAsciiByteString #-}

uniformAsciiByteStringSimple
    :: ( PrimMonad base
       , MonadBase base m
       , MonadReader env m
       , HasMWC env (PrimState base)
       )
    => Int -- ^ length
    -> m ByteString
uniformAsciiByteStringSimple = liftM fromByteVector . flip VS.replicateM uniformAsciiByte
{-# INLINE uniformAsciiByteStringSimple #-}

uniformAsciiByteStringComplex64
    :: ( PrimMonad base
       , MonadBase base m
       , MonadReader env m
       , HasMWC env (PrimState base)
       )
    => Int -- ^ length
    -> m ByteString
uniformAsciiByteStringComplex64 len = do
    gen <- liftM getMWC ask
    liftBase $ do
        v <- VM.new len
        let loop _ _ i | i >= len = finish
            loop _ 0 i = do
                w <- uniform gen
                loop (w :: Int) 16 i -- FIXME only 8 on 32 bit machines
            loop w rem i = do
                let x = w .&. 0xF
                    w' = w `shiftR` 4
                VM.unsafeWrite v i (V.unsafeIndex table x)
                loop w' (rem - 1) (i + 1)

            finish = liftM fromByteVector (VS.unsafeFreeze v)
        loop 0 0 0
{-# INLINE uniformAsciiByteStringComplex64 #-}

-- | Same as 'uniformAsciiByteString', but generates a 'Text'.
uniformAsciiText
    :: ( PrimMonad base
       , MonadBase base m
       , MonadReader env m
       , HasMWC env (PrimState base)
       )
    => Int -- ^ length
    -> m Text
uniformAsciiText = liftM decodeUtf8 . uniformAsciiByteString
{-# INLINE uniformAsciiText #-}

uniformAsciiTextSimple
    :: ( PrimMonad base
       , MonadBase base m
       , MonadReader env m
       , HasMWC env (PrimState base)
       )
    => Int -- ^ length
    -> m Text
uniformAsciiTextSimple = liftM decodeUtf8 . uniformAsciiByteStringSimple
{-# INLINE uniformAsciiTextSimple #-}

uniformAsciiTextComplex64
    :: ( PrimMonad base
       , MonadBase base m
       , MonadReader env m
       , HasMWC env (PrimState base)
       )
    => Int -- ^ length
    -> m Text
uniformAsciiTextComplex64 = liftM decodeUtf8 . uniformAsciiByteStringComplex64
{-# INLINE uniformAsciiTextComplex64 #-}

-- Utility functions

table :: V.Vector Word8
table =
    V.generate 64 go
  where
    go w
        | w < 26 = fromIntegral w + _A
        | w < 52 = fromIntegral w + _a - 26
        | w < 62 = fromIntegral w + _0 - 52
        | w == 62 = _hyphen
        | w == 63 = _underscore
{-# NOINLINE table #-}

w2ascii :: Word8 -> Word8
w2ascii = V.unsafeIndex table . fromIntegral

high :: Word8 -> Word8
high = (`shiftR` 4)

low :: Word8 -> Word8
low = (.&. 0xF)
