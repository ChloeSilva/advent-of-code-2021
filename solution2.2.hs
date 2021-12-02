import System.IO 

main = do  
    handle <- openFile "input2.txt" ReadMode  
    contents <- hGetContents handle
    print $ position (words contents) 0 0 0
    hClose handle

position :: [String] -> Int -> Int -> Int -> Int
position (d:x:xs) a b c | d == "forward" = position xs (a+n) (c*n+b) c
                        | d == "down"    = position xs a b (c+n)
                        | d == "up"      = position xs a b (c-n)
                          where n = read x :: Int
position [] a b _ = a * b