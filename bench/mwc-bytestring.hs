import Criterion.Main
import Control.Monad.MWC
import Control.Monad.Reader

main :: IO ()
main = do
    gen <- createSystemRandom
    let wrap name f size
            = bench name $ whnfIO $ replicateM_ 10000
            $ runReaderT (f size) gen
    defaultMain $ flip map [10, 100, 1000, 10000] $ \size -> bgroup (show size)
        [ wrap "simple replicateM" uniformAsciiByteStringSimple size
        , wrap "complex Word64" uniformAsciiByteStringComplex64 size
        ]
