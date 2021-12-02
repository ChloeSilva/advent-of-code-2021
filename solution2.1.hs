import System.IO 

main = do  
    handle <- openFile "input2.txt" ReadMode  
    contents <- hGetContents handle
    print $ position (words contents) 0 0
    hClose handle

position :: [String] -> Int -> Int -> Int
position (d:x:xs) a b | d == "forward" = position xs (a+n) b
                      | d == "down"    = position xs a (b+n)
                      | d == "up"      = position xs a (b-n)
                        where n = read x :: Int
position [] a b = a * b