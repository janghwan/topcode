import Control.Monad
import Text.Printf
import Data.List

solution :: Double -> String
solution a = let theta = asin (a ** 2 - 1) /2
                 (x1, y1) = (-0.5 * (sin theta), 0.5 * (cos theta))
                 (x2, y2) = (0.5 * (cos theta), 0.5 * (sin theta))
             in (printf "%f %f 0.0\n%f %f 0.0\n0.0 0.0 0.5" x1 y1 x2 y2) :: String

main = enumFromTo (1::Int) <$> readLn >>= mapM_ doCase where
    doCase i = do
        a  <- read getLine :: Double
        if a > sqrt 2
        then putStrLn $ (printf "Case #%d:\n" i) ++ (solution (read a::Double))
        else putStrLn $ (printf "Case #%d:\n" i) ++ (solution (read a::Double))

