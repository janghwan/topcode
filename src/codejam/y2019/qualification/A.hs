
import Control.Applicative
import Data.List


solution :: String -> String
solution n = (show $ (read a :: Integer)) ++ " " ++ (show $ (read b :: Integer))
            where (a, b) = unzip $ sol n

sol :: String -> [(Char, Char)]
sol [] = []
sol ('4' : xs) = ('1', '3') : sol xs
sol (x : xs) = (x, '0') : sol xs

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
    where doCase caseno = do
                        n <- zgetLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                (solution n)