{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | Universal IO streaming interface, inspired by:
-- http://www.reddit.com/r/haskell/comments/2o5558/is_network_library_poorly_implemented_or_am_i/cmjvwye
module Data.Streaming.Universal where

import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified System.IO as IO
import Data.IORef
import Data.Monoid (Monoid, mappend, mempty)
import Conduit

-- | Usually EOF is indicated by some empty data, e.g. 0-length bytestring.
class IndicatesEOF a where
    type UnwrappedEOF a
    indicatesEOF :: a -> Bool
    -- | If this is not an EOF, unwrap it.
    unwrapEOF :: a -> UnwrappedEOF a
instance IndicatesEOF S.ByteString where
    type UnwrappedEOF S.ByteString = S.ByteString
    indicatesEOF = S.null
    unwrapEOF = id
instance IndicatesEOF (Maybe a) where
    type UnwrappedEOF (Maybe a) = a
    indicatesEOF = maybe True (const False)
    unwrapEOF (Just x) = x

class IndicatesEOF (InputStreamElem s) => InputStream s where
    type InputStreamElem s
    isRead :: s -> IO (InputStreamElem s)
class InputStream s => InputStreamPutBack s where
    -- | Invariant: never put back something which indicates EOF.
    isPutBack :: s -> InputStreamElem s -> IO ()
class InputStream s => InputStreamClose s where
    isClose :: s -> IO ()

instance InputStream IO.Handle where
    type InputStreamElem IO.Handle = S.ByteString
    isRead = flip S.hGetSome defaultChunkSize
instance InputStreamClose IO.Handle where
    isClose = IO.hClose

data WrappedHandle = WrappedHandle IO.Handle !Int
instance InputStream WrappedHandle where
    type InputStreamElem WrappedHandle = S.ByteString
    isRead (WrappedHandle h size) = S.hGetSome h size
instance InputStreamClose WrappedHandle where
    isClose (WrappedHandle h _) = IO.hClose h

data PutBack s = PutBack s (IO (InputStreamElem s)) (InputStreamElem s -> IO ())
instance InputStream s => InputStream (PutBack s) where
    type InputStreamElem (PutBack s) = InputStreamElem s
    isRead (PutBack _ read' _) = read'
instance InputStream s => InputStreamPutBack (PutBack s) where
    isPutBack (PutBack _ _ f) = f
instance InputStreamClose s => (InputStreamClose (PutBack s)) where
    isClose (PutBack s _ _) = isClose s

mkPutBack :: InputStream s => s -> IO (PutBack s)
mkPutBack s = do
    ref <- newIORef []
    let read' = do
            list <- readIORef ref
            case list of
                x:y -> do
                    writeIORef ref y
                    return x
                [] -> isRead s
        putBack x = modifyIORef ref (x:)
    return (PutBack s read' putBack)

mkPutBackMonoid :: (Monoid (InputStreamElem s), InputStream s) => s -> IO (PutBack s)
mkPutBackMonoid s = do
    ref <- newIORef mempty
    let read' = do
            val <- readIORef ref
            if indicatesEOF val
                then isRead s
                else do
                    writeIORef ref mempty
                    return val
        putBack x = modifyIORef ref (x `mappend`)
    return (PutBack s read' putBack)

sourceInputStream :: (MonadIO m, InputStream s)
                  => s
                  -> Producer m (UnwrappedEOF (InputStreamElem s))
sourceInputStream s =
    loop
  where
    loop = do
        res <- liftIO $ isRead s
        if indicatesEOF res
            then return ()
            else do
                yield (unwrapEOF res)
                loop

main :: IO ()
main = sourceInputStream IO.stdin $$ mapM_C print
