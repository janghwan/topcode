import Control.Monad
-- this solution is wrong
solution n k v attr = take k (drop from (attr++attr)) where
                        from = mod v k


main = enumFromTo 1 <$> readLn >>= mapM_ doCase
  where doCase caseno = do
                        [n, k, v] <- (\x -> read <$> words x) <$> getLine
                        attractions <- replicateM n getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                                (unwords $ solution n k v attractions)