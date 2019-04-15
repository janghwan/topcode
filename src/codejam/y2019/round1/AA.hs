
import Control.Applicative
import Data.List
import qualified Data.Set as Set
import Control.Monad
import Debug.Trace

solution :: [String] -> Int
solution words = let list = reverse.sort $ fmap reverse words in length words - (length $ sol list Set.empty)

sol sortedWords set =
        if length longestPrefix > 0
        then sol (remove sortedWords longestPrefix 2 []) (Set.insert longestPrefix set)
        else sortedWords
            where longestPrefix = findLongest sortedWords "" set

findLongest [] prefix set = prefix
findLongest (x:[]) prefix set = prefix
findLongest (x:y:xs) prefix set =
    if (Set.notMember newPrefix set && length newPrefix > length prefix)
    then findLongest (y:xs) newPrefix set
    else findLongest (y:xs) prefix set where
        newPrefix = lcp x y ""

remove xs prefix 0 res = (reverse res) ++ xs
remove (x:xs) prefix n res
    | startsWith x prefix = remove xs prefix (n-1) res
    | otherwise = remove xs prefix n (x:res)


startsWith x [] = True
startsWith (x:xs) (p:ps)
    | x == p = startsWith xs ps
    | otherwise = False

lcp :: String -> String -> String -> String
lcp [] _ p = reverse p
lcp _ [] p = reverse p
lcp (a:aa) (b:bb) p
    | a == b = lcp aa bb (a:p)
    | otherwise = reverse p

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
    where doCase caseno = do
                        n <- readLn
                        words <- replicateM n getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                (show $ solution words)