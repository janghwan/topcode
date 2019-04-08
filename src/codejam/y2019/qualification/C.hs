
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Debug.Trace

filt [] = []
filt (x:[]) = [x]
filt (x:y:xs)
    | x == y =  filt (x:xs)
    | otherwise = x : filt(y:xs)

solution :: [Integer] -> String
solution encoded@(a:b:l) =  (map (alphabet Map.!) decoded) where
    unique = filt encoded
    [first, _, _] = tripple a b where (a:b:_) = unique
    decoded = reverse $ foldl fact [first] (a:b:l)
    primes = (sort $ nub $ foldl fact [x `div` (gcd x y)] encoded) where (x:y:_) = unique
    fact (x:xs) a = (a `div` x) : x : xs
    alphabet = (Map.fromList $ zip primes "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    tripple a b = [a `div` d, d, b `div` d] where d = gcd' a b
    gcd' a b
        | a == b = case (find (\x -> x * x == a) primes) of
                    Just x -> x
                    Nothing -> fromMaybe 0 (find (\x -> a `mod` x == 0) primes)
        | otherwise = gcd a b

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
    where doCase caseno = do
                        _ <- getLine
                        l <-  map read . words <$> getLine :: IO [Integer]
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                (solution l)