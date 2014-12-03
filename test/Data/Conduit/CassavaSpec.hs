{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.CassavaSpec (spec) where

import Test.Hspec
import ClassyPrelude.Conduit
import Data.Conduit.Cassava
import qualified Data.Csv as C

spec :: Spec
spec = do
    describe "max bytes works" $ do
        it "decode" $ do
            let src = forever $ yield "foobarbaz,"
            (src $$ decode (Just 10) defaultDecodeOptions NoHeader =$ ignoreRecords) `shouldThrow`
                (== LineTooLong)
        it "decodeHeader" $ do
            let src = forever $ yield "foobarbaz,"
            (src $$ decodeHeader (Just 10) defaultDecodeOptions) `shouldThrow`
                (== LineTooLong)
    it "encode/decode is idempotent" $ do
        res <- yieldMany sampleScores
            $$ encode defaultEncodeOptions
            =$ decode Nothing defaultDecodeOptions NoHeader
            =$ sinkList
        res `shouldBe` sampleScores
    it "encodeByName/decodeByName is idempotent" $ do
        res <- yieldMany sampleScores
            $$ encodeByName defaultEncodeOptions scoreHeader
            =$ decodeByName Nothing defaultDecodeOptions
            =$ sinkList
        res `shouldBe` sampleScores

ignoreRecords :: Monad m => Consumer (Vector Text) m ()
ignoreRecords = sinkNull

data Scores = Scores !Int !Int !Int
    deriving (Show, Eq)

scoreHeader :: Header
scoreHeader = pack ["score2", "score3", "score1"]

sampleScores :: [Scores]
sampleScores = zipWith3 Scores [1..1000] [1,3..] [2,5..]

instance ToRecord Scores where
    toRecord (Scores x y z) = toRecord (x, y, z)
instance FromRecord Scores where
    parseRecord = fmap (\(x, y, z) -> Scores x y z) . parseRecord
instance ToNamedRecord Scores where
    toNamedRecord (Scores x y z) = toNamedRecord $ asHashMap $ mapFromList
        [ ("score1" :: ByteString, x)
        , ("score2", y)
        , ("score3", z)
        ]
instance FromNamedRecord Scores where
    parseNamedRecord x = Scores
        <$> C.lookup x "score1"
        <*> C.lookup x "score2"
        <*> C.lookup x "score3"
