import Control.Monad

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
  where doCase caseno = do
                        n <- readLn
                        _ <- replicateM (n+1) getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                            (if even n then "0" else "1\n0.0")
