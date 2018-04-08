module Sample where

import Control.Applicative
import Data.List

test ((ax,ay),(bx,by),(cx,cy)) = (ax+bx+cx) `mod` 3 == 0 && (ay+by+cy) `mod` 3 == 0

type Point = (Integer,Integer)
type Triangle = (Point,Point,Point)

triangles :: [Point] -> [Triangle]
triangles xs = do
                (a:as) <- tails (nub xs)
                (b:bs) <- tails as
                c <- bs
                return (a,b,c)

solution n a b c d x y m = length . filter test . triangles . nub $
        take (fromInteger n) (iterate (points a b c d m) (x,y)) where
        points a b c d m (x,y) = ((a * x + b) `mod` m ,(c * y + d) `mod` m)

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
  where doCase caseno = do
                        [n,a,b,c,d,x,y,m] <- map read . words <$> getLine --  . compose
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                show (solution n a b c d x y m)