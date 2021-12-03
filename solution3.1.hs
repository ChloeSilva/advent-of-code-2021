import System.IO
import Data.List
import Data.Char

main = do  
    handle <- openFile "input3.txt" ReadMode  
    contents <- hGetContents handle
    let gamma = getGamma (transpose $ words contents)
    let epsilon = bitFlip gamma
    print $ (binToInt gamma) * (binToInt epsilon)
    hClose handle

getGamma :: [String] -> String
getGamma = map (\xs -> if  start '1' xs > start '0' xs then '1' else '0')
               where start = (\x -> length . filter (== x))

bitFlip :: [Char] -> [Char]
bitFlip = map (\x -> if (x == '1') then '0' else '1')

binToInt :: String -> Int
binToInt = foldl (\a x -> a * 2 + digitToInt x) 0