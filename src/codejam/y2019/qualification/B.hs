
import Control.Applicative
import Data.List

sol :: String -> String -> String
sol [] ys = ys
sol ('S' : xs) ys = sol xs ('E' : ys)
sol ('E' : xs) ys = sol xs ('S' : ys)

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
    where doCase caseno = do
                        _ <- getLine
                        s <- getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                (reverse $ sol s [])