
import Control.Applicative
import Data.List

damage xs = helper xs 1
            where helper (x:xs) d
                        | x == 'C' = helper xs (d * 2)
                        | x == 'S' = d + helper xs d
                  helper [] _ = 0
countC xs = length $ filter (=='C') xs


hack p = foldr step ("", 0) p
        where
            step c ("", rd) = (c:"", rd)
            step c (x:xs, rd)
                | x == 'S' && c == 'C' && rd == 0 = ( x:c:xs, floor (maxD/(2^(countC xs + 1))))
                | otherwise            = ( c:x:xs, rd)
                where maxD = 2 ^ (countC p)

solution :: Int -> String -> Maybe Int
solution d p = helper p 0 0
                where helper xs dr count
                        | dTotal - dr <= d = Just count
                        | otherwise = let (xs', dr') = hack xs
                                      in if dr' == 0
                                         then Nothing
                                         else helper xs' (dr + dr') (count + 1)
                         where dTotal = damage p


main = enumFromTo 1 <$> readLn >>= mapM_ doCase
  where doCase caseno = do
                        [d, p] <- words <$> getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                maybe "IMPOSSIBLE" show (solution (read d) p)