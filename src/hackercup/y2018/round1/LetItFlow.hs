import Control.Monad

data Pos = Top | Mid

solve :: Pos -> Integer -> [(Char,Char,Char)] -> Integer

solve Mid count   ((_  ,_  ,'.'):[])              = count
solve _   count    (_           :[])              = 0
solve Mid count m@(('.',_  ,'.'):('.',_  ,'.'):_) = solve Top ((count*2) `mod` 1000000007) (tail m)
solve Mid count m@(('.',_  ,_  ):('.',_  ,_  ):_) = solve Top count (tail m)
solve Mid count m@((_  ,_  ,'.'):(_  ,_  ,'.'):_) = solve Top count (tail m)
solve _   count m@((_  ,'.',_  ):(_  ,'.',_  ):_) = solve Mid count (tail m)
solve _   _     _                                 = 0

main = enumFromTo 1 <$> readLn >>= mapM_ doCase
  where doCase caseno = do
                        _ <- getLine
                        t <- getLine
                        m <- getLine
                        b <- getLine
                        putStrLn $ "Case #" ++ show caseno ++ ": " ++
                            show (solve Top 1 (zip3 t m b))
