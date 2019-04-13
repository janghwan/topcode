import Control.Monad
import Data.Array as Array
import Data.Set as Set
import Data.List as List
import Data.Maybe
import Debug.Trace

preOrder 0 _ = []
preOrder i tree = i : preOrder left tree ++ preOrder right tree where
        (left, right) = tree!i

postOrder 0 _ = []
postOrder i tree = postOrder left tree ++ postOrder right tree ++ [i] where
        (left, right) = tree!i

merge'' :: [Set Int] -> [Set Int]
merge'' sets = let merged = merge' sets in
                    if length merged == length sets then merged else merge'' merged
merge' []= []
merge' (x:xs) = let inters = List.filter ((>0) . size . (intersection x)) xs
                    others = List.filter ((==0) . size . (intersection x)) xs
                in
                    unions (x:inters) : merge' others

findLabel sets i = (+1) <$> List.findIndex (member i) sets

shrink part k = List.take (k - 1) part ++ [unions (List.drop (k-1) part)]

solve tree n k = let
                    pre = preOrder 1 tree
                    post = postOrder 1 tree
                    sets = List.map (\(x,y) -> fromList [x, y] ) (zip pre post)
                    part = merge'' sets
                 in
--                     trace ((show (zip pre post)) ++ (show part)) $
                    if length part < k then "Impossible"
                    else
                        intercalate " " $ List.map show $ catMaybes $ List.map (findLabel $ shrink part k) [1..n]

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
  where doCase caseno = do
                        [n, k] <- (\x -> read <$> words x) <$> getLine
                        tree <- replicateM n $ (\x -> (\([x,y]) -> (x,y)) $ read <$> words x) <$> getLine :: IO [(Int, Int)]
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                             solve (listArray (1, n) tree) n k
