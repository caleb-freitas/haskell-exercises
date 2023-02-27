main :: Int -> [Char] -> IO ()
main k ns = mapM_ print (concatMap (replicate k . read) (words ns) :: [Int])
