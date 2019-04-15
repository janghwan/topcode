
import Control.Applicative
import Data.List
import qualified Data.Set as Set
import Control.Monad
import Debug.Trace

solution :: [String] -> Int
solution words = sol words suffixList 0 where
    suffixList = reverse $ sortBy sortGT $ fmap (\x -> (length x, x)) (Set.toList $ suffixSet words)
    sortGT (len1, _) (len2, _) = compare len1 len2

suffixSet :: [String] -> Set.Set String
suffixSet words = foldl findSuffix Set.empty pairs where
    pairs = [(a,b) | a <- words, b <- words]
    findSuffix set (a,b) = let cs = lcs a b in
        if a == b || length cs == 0 then set else Set.insert cs set

lcs a b = let
    sa = length a
    sb = length b in
    if sa > sb then lcs' (slice (sa - sb) sa a) b "" else lcs' a (slice (sb - sa) sb b) ""

slice start end = take (end-start) . drop start

endsWith :: String -> String -> Bool
endsWith suffix word = drop (length word - length suffix) word == suffix

lcs' [] [] zs = zs
lcs' (x:xs) (y:ys) zs
    | x == y = lcs' xs ys (zs ++ [x])
    | otherwise = lcs' xs ys []

sol [] _ count = count
sol _ [] count = count
sol words ((len, suffix):sList) count = sol filtered sList (count + 2)
    where filtered = remove (endsWith suffix) words 2

remove :: (String -> Bool) -> [String] -> Int -> [String]
remove pred xs 0 = xs
remove pred (x:xs) n
    | pred x == True = remove pred xs (n-1)
    | otherwise = x : remove pred xs (n-1)

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
    where doCase caseno = do
                        n <- readLn
                        words <- replicateM n getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                (show $ solution words)