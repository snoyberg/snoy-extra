{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
-- | conduit adapter for cassava
--
-- One major API difference: this module allows you to set a maximum number of
-- bytes to be read per line. Due to how the Cassava API works and for chunking
-- optimizations, this number won't be an exact ceiling, but will prevent
-- complete runaway memory usage.
module Data.Conduit.Cassava
    ( CassavaException (..)
    , C.Header
    , C.HasHeader (..)
    , C.ToRecord (..)
    , C.FromRecord (..)
    , C.ToNamedRecord (..)
    , C.FromNamedRecord (..)
    , C.ToField (..)
    , C.FromField (..)
    , C.DecodeOptions (..)
    , C.defaultDecodeOptions
    , C.EncodeOptions (..)
    , C.defaultEncodeOptions
    , decode
    , decodeHeader
    , decodeByName
    , decodeWithHeaders
    , encode
    , encodeByName
    ) where

import qualified Data.Csv as C hiding (decodeWith)
import qualified Data.Csv.Incremental as C
import ClassyPrelude.Conduit

-- | An approximate maximum of bytes per record to allow. When violated,
-- functions will throw 'LineTooLong'.
type MaxBytes = Maybe Int

data CassavaException = CassavaException Text
                      | LineTooLong
                      | MissingNamedField ByteString
    deriving (Show, Typeable, Eq)
instance Exception CassavaException

-- | Decode from unnamed records.
decode :: (C.FromRecord a, MonadThrow m)
       => MaxBytes
       -> C.DecodeOptions
       -> C.HasHeader -- ^ Data contains header that should be skipped
       -> Conduit ByteString m a
decode mbytes0 opts hasHeader =
    loop mbytes0 (C.decodeWith opts hasHeader)
  where
    loop (Just bytes) _ | bytes <= 0 = throwM LineTooLong
    loop _ (C.Fail bs err) = leftover bs >> throwM (CassavaException $ pack err)
    loop _ (C.Done recs) = yieldRecs recs
    loop mbytes (C.Many recs f) = do
        yieldRecs recs
        bs <- fromMaybe mempty <$> await
        let mbytes'
                | null recs = (`subtract` length bs) <$> mbytes
                | otherwise = mbytes0
        loop mbytes' (f bs)

    yieldRecs = mapM_ (either (throwM . CassavaException . pack) yield)

-- | Get the headers.
decodeHeader :: MonadThrow m
             => MaxBytes
             -> C.DecodeOptions
             -> Consumer ByteString m C.Header
decodeHeader mbytes0 opts =
    loop mbytes0 (C.decodeHeaderWith opts)
  where
    loop (Just bytes) _ | bytes <= 0 = throwM LineTooLong
    loop _ (C.FailH bs err) = leftover bs >> throwM (CassavaException $ pack err)
    loop _ (C.DoneH h bs) = leftover bs >> return h
    loop mbytes (C.PartialH f) = do
        bs <- fromMaybe mempty <$> await
        let mbytes' = (`subtract` length bs) <$> mbytes
        loop mbytes' (f bs)

-- | Parse named records using the supplied header.
decodeWithHeaders :: (C.FromNamedRecord a, MonadThrow m)
                  => MaxBytes
                  -> C.DecodeOptions
                  -> C.Header
                  -> Conduit ByteString m a
decodeWithHeaders maxBytes opts header =
    decode maxBytes opts C.NoHeader =$= mapMC go
  where
    go record =
        case C.runParser $ C.parseNamedRecord m of
            Left err -> throwM $ CassavaException $ pack err
            Right v  -> return v
      where
        m = mapFromList $ zip (unpack header) (unpack (record :: C.Record))

-- | Parse named records by first parsing the headers from the stream.
decodeByName :: (C.FromNamedRecord a, MonadThrow m)
             => MaxBytes
             -> C.DecodeOptions
             -> Conduit ByteString m a
decodeByName maxBytes opts = do
    header <- decodeHeader maxBytes opts
    decodeWithHeaders maxBytes opts header

-- | Encode unnamed records.
--
-- Note: If Cassava exposed its Builder internals, this could be much faster.
encode :: (Monad m, C.ToRecord a) => C.EncodeOptions -> Conduit a m ByteString
encode opts = awaitForever (yieldMany . toChunks . C.encodeWith opts . return)

-- | Encode named records.
--
-- Note: If Cassava exposed its Builder internals, this could be much faster.
encodeByName :: (MonadThrow m, C.ToNamedRecord a)
             => C.EncodeOptions
             -> C.Header
             -> Conduit a m ByteString
encodeByName opts header = do
    yield header =$= encode opts
    mapMC (go . C.toNamedRecord) =$= encode opts
  where
    go nr =
        flip mapM header $ \n ->
            case lookup n nr of
                Nothing -> throwM $ MissingNamedField n
                Just v  -> return v
