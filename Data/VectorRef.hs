-- | Use 1-length mutable vectors for mutable references.
--
-- Motivated by: http://stackoverflow.com/questions/27261813/why-is-my-little-stref-int-require-allocating-gigabytes and ArrayRef.
--
-- Focuses on unboxed vectors, as that's the most likely use case, but supports
-- /any/ vector implementation.
module Data.VectorRef
    ( -- * Types
      GVRef
    , VRef
    , IOVRef
    , STVRef
      -- * Functions
    , newGVRef
    , newVRef
    , readVRef
    , writeVRef
    , modifyVRef
    , modifyVRef'
    ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST)
import Control.Monad (liftM)
import qualified Data.Vector.Unboxed.Mutable as VU
import qualified Data.Vector.Storable.Mutable as VS
import qualified Data.Vector.Mutable as VB
import qualified Data.Vector.Generic.Mutable as V

-- | A generic vector reference, supporting any kind of underlying vector in
-- any monad.
newtype GVRef v s a = GVRef (v s a)

-- | An unboxed vector reference, supporting any monad.
type VRef = GVRef VU.MVector

-- | An unboxed IO vector reference.
type IOVRef = VRef (PrimState IO)

-- | An unboxed ST vector reference.
type STVRef = VRef

-- | Create a new vector reference, supporting any kind of underlying vector.
newGVRef :: (V.MVector v a, PrimMonad m) => a -> m (GVRef v (PrimState m) a)
newGVRef = liftM GVRef . V.replicate 1
{-# INLINE newGVRef #-}

-- | Create a new unboxed vector reference.
newVRef :: (VU.Unbox a, PrimMonad m) => a -> m (VRef (PrimState m) a)
newVRef = newGVRef
{-# INLINE newVRef #-}

-- | Read a reference.
readVRef :: (V.MVector v a, PrimMonad m) => GVRef v (PrimState m) a -> m a
readVRef (GVRef v) = V.unsafeRead v 0
{-# INLINE readVRef #-}

-- | Write to a reference.
writeVRef :: (V.MVector v a, PrimMonad m) => GVRef v (PrimState m) a -> a -> m ()
writeVRef (GVRef v) = V.unsafeWrite v 0
{-# INLINE writeVRef #-}

-- | Modify a reference. This is a non-atomic action. This /may/ not evaluate
-- the thunk, depending on the underlying vector. In the case of unboxed or
-- storable vectors, the thunk is guaranteed to be evaluated.
modifyVRef :: (V.MVector v a, PrimMonad m)
           => GVRef v (PrimState m) a
           -> (a -> a)
           -> m ()
modifyVRef (GVRef v) f = V.unsafeRead v 0 >>= V.unsafeWrite v 0 . f
{-# INLINE modifyVRef #-}

-- | Same as 'modifyVRef', but guarantees that the thunk will be evaluated. For
-- unboxed and storable vectors, this function provides no benefits over
-- @modifyVRef@.
modifyVRef' :: (V.MVector v a, PrimMonad m)
            => GVRef v (PrimState m) a
            -> (a -> a)
            -> m ()
modifyVRef' (GVRef v) f = do
    x <- V.unsafeRead v 0
    let y = f x
    y `seq` V.unsafeWrite v 0 y
