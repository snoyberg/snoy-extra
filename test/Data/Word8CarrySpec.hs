{-# LANGUAGE MagicHash #-}
module Data.Word8CarrySpec (spec) where

import Data.Word             (Word, Word8)
import Data.Word8Carry
import Test.Hspec            (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

implementations :: [(String, Word8 -> Word8 -> (Word8, Bool))]
implementations =
    [ ("boxed", boxed)
    , ("primops", primops)
    ]

spec :: Spec
spec = do
    mapM_ mkDescribe pairs
    mapM_ mkProp implementations
  where
    pairs =
        [ (0, 0)
        , (255, 255)
        , (16, 255)
        , (31, 21)
        ]

    mkDescribe (x, y) =
        describe (show (x, y)) $ mapM_ mkTest implementations
      where
        mkTest (name, f) = it name $
            let (z, overflow) = f x y
                expected = fromIntegral x + fromIntegral y
                actual = fromIntegral z + (if overflow then 256 else 0)
             in expected `shouldBe` (actual :: Word)

    mkProp (name, f) = prop name $ \(x, y) ->
            let (z, overflow) = f x y
                expected = fromIntegral x + fromIntegral y
                actual = fromIntegral z + (if overflow then 256 else 0)
             in expected `shouldBe` (actual :: Word)
