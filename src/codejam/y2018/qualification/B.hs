import Control.Monad
import Text.Printf
import Data.List

splits [] = ([], [])
splits (x:[]) = ([x], [])
splits (x:y:xs) = let (as, bs) = splits xs
                    in (x:as, y:bs)

solution :: Int -> [Int] -> Maybe Int
solution n vs = let (o, e) = splits vs
                    zipped = zip (sort o) (sort e)
                    zipped' = zip (tail $ sort o) (sort e)
                    oa = fmap (\x -> (x *2) ) $ findIndex (\(x, y) -> x > y) zipped
                    ob = fmap (\x -> (x+1)*2 - 1) $ findIndex (\(x, y) -> x < y) zipped'
                in case (oa, ob) of
                    (Just x, Just y) -> Just (minimum [x,y])
                    (Just x, Nothing) -> Just x
                    (Nothing, Just y) -> Just y
                    _ -> Nothing

main = enumFromTo (1::Int) <$> readLn >>= mapM_ doCase where
    doCase i = do
        n <- readLn
        vs <- map read . words <$> getLine :: IO [Int]
        putStrLn $ printf "Case #%d: " i ++ maybe "OK" show (solution n vs)
