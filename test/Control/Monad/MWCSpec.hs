{-# LANGUAGE ViewPatterns #-}
module Control.Monad.MWCSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Text as T
import Control.Monad.MWC
import Control.Monad (forM_)

spec :: Spec
spec = do
    prop "valid byte" $ do
        w <- runMWCReaderT uniformAsciiByte
        shouldSatisfy (toEnum $ fromEnum w) isValid
    let impls =
            [ ("standard", uniformAsciiText)
            , ("simple", uniformAsciiTextSimple)
            , ("complex64", uniformAsciiTextComplex64)
            ]
    forM_ impls $ \(name, f) -> describe name $ do
        prop "correct length" $ \(abs -> len) -> do
            t <- runMWCReaderT $ f len
            T.length t `shouldBe` len
        prop "correct content" $ \(abs -> len) -> do
            t <- runMWCReaderT $ f len
            shouldSatisfy t (T.all isValid)
  where
    isValid c =
        ('A' <= c && c <= 'Z') ||
        ('a' <= c && c <= 'z') ||
        ('0' <= c && c <= '9') ||
        c == '-' ||
        c == '_'
