module Codewars.G964.Sumdigpow where

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = let range = [a..b]
                in (\(x, _) -> x) <$>
                     (filter (\(_, y) -> y) $
                     (\x -> (x, check x x $ length $ show x)) <$>
                     range)

check i j n
    | n == 0 && j == 0 = True
    | n == 0 = False
    | otherwise = check i j' (n - 1) where
        digits = length $ show i
        m = digits - n + 1
        square = ((i `mod` 10^n) `div` 10^(n-1))^m
        j' = j - square
