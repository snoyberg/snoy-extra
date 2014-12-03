module Data.VectorRefSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.VectorRef
import Data.IORef
import Control.Monad (forM_)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (oneof)

data Action = Write Int
            | Add Int
            | Subtract Int
    deriving (Show, Eq)

instance Arbitrary Action where
    arbitrary = oneof
        [ fmap Write arbitrary
        , fmap Add arbitrary
        , fmap Subtract arbitrary
        ]

spec :: Spec
spec = prop "random actions work like IORef" $ \start actions -> do
    vref <- newVRef start
    ioref <- newIORef start
    let check = do
            x <- readVRef vref
            y <- readIORef ioref
            x `shouldBe` y
        perform (Write x) = do
            writeVRef vref x
            writeIORef ioref x
        perform (Add x) = do
            modifyVRef vref (+ x)
            modifyIORef' ioref (+ x)
        perform (Subtract x) = do
            modifyVRef vref (`subtract` x)
            modifyIORef' ioref (`subtract` x)
    check
    forM_ actions $ \act -> perform act >> check
