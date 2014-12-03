{-# LANGUAGE MagicHash #-}
import Criterion.Main        (bench, bgroup, defaultMain, nf)
import Data.Word8Carry
import Data.Word (Word8)

implementations :: [(String, Word8 -> Word8 -> (Word8, Bool))]
implementations =
    [ ("boxed", boxed)
    , ("primops", primops)
    ]

main :: IO ()
main =
    defaultMain $ map mkGroup pairs
  where
    pairs =
        [ (0, 0)
        , (255, 255)
        , (16, 255)
        , (31, 21)
        ]

    mkGroup (x, y) =
        bgroup (show (x, y)) $ map mkBench implementations
      where
        mkBench (name, f) = bench name $ nf (uncurry f) (x, y)
