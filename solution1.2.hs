import System.IO 

main = do  
    handle <- openFile "input1.txt" ReadMode  
    contents <- hGetContents handle
    let integers =  map read $ words contents :: [Int]
    print $ countIncrease (integers, 0)
    hClose handle

countIncrease :: ([Int], Int) -> Int
countIncrease ((a:b:c:d:xs), i) | a+b+c < b+c+d = countIncrease ((b:c:d:xs), i+1)
                                | otherwise     = countIncrease ((b:c:d:xs), i)
countIncrease ([a,b,c], i) = i