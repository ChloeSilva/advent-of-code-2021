import System.IO
import Data.List
import Data.Char

main = do  
    handle <- openFile "input3.txt" ReadMode  
    contents <- hGetContents handle
    let co2 =  getCO2 [] (words contents)
    let o = getO [] (words contents)
    print $ (binToInt o) * (binToInt co2)
    hClose handle

getCO2 :: String -> [String] -> String
getCO2 x [y] = x ++ y
getCO2 x xs | startWith '1' h < startWith '0' h = getCO2 (x++"1") (filterWith '1' xs)
            | otherwise                         = getCO2 (x++"0") (filterWith '0' xs)
                where h = map head xs

getO :: String -> [String] -> String
getO x [y] = x ++ y
getO x xs | startWith '1' h >= startWith '0' h = getO (x++"1") (filterWith '1' xs)
          | otherwise                          = getO (x++"0") (filterWith '0' xs)
                where h = map head xs

startWith :: Char -> [Char] -> Int
startWith x xs = length(filter (== x) xs)

filterWith :: Char -> [String] -> [String]
filterWith x xs =  map tail (filter (\a -> head a == x) xs)

binToInt :: String -> Int
binToInt = foldl (\c x -> c * 2 + digitToInt x) 0