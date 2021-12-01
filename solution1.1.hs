import System.IO 

main = do  
    handle <- openFile "input1.txt" ReadMode  
    contents <- hGetContents handle
    let integers =  map read $ words contents :: [Int]
    print $ countIncrease (integers, 0)
    hClose handle

countIncrease :: ([Int], Int) -> Int
countIncrease ((x:y:xs), c) | x < y     = countIncrease ((y:xs), c+1)
                            | otherwise = countIncrease ((y:xs), c)
countIncrease ([x],c) = c