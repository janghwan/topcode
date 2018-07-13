import Control.Monad
import Data.List

solution (x:xs) = let (pre, post) = break (==x) xs in
                    if repeating (x:pre) post then "Impossible" else x:pre ++ x:xs where
                        repeating _ [] = True
                        repeating a b | a `isPrefixOf` b = repeating a (drop (length a) b)
                                      | b `isPrefixOf` a = True
                        repeating _ _ = False

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
  where doCase caseno = do
                        str <- getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                solution str