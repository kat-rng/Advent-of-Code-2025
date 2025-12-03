import Data.List
import System.IO

-- Kept for pt1 compatibility
maxJolt x = maximum x

-- Kept for pt1 compatibility
findIdx :: Char -> String -> Int
findIdx x xs = case i of 
    Just i -> i
    Nothing -> error "Jolt value missing" 
    where i = elemIndex x xs

-- drops everything up to and including the first occurence of x
dropIdx :: Char -> String -> String
dropIdx x xs = case i of 
    Just i -> drop (i+1) xs
    Nothing -> error "Jolt value missing" 
    where i = elemIndex x xs

-- Finds the jolt options
joltOptions :: Int -> String -> String
joltOptions joltsLeft xs = drop (joltsLeft-1) (reverse xs)

-- Finds the max of the options
maxJoltHere :: Int -> String -> Char
maxJoltHere n xs = maximum $ joltOptions n xs

-- Finds the maximum combination of joltages
maxJoltCombo :: String -> Int -> String -> Int
maxJoltCombo xs n c = case n of
    0 -> read c
    _ -> maxJoltCombo newXs (n-1) (c++[maxJ])
    where 
        maxJ  = maxJoltHere n xs
        newXs = dropIdx maxJ xs

newMaxJoltCombo :: String -> Int
newMaxJoltCombo xs = maxJoltCombo xs 12 ""


pt1 = do
    -- Reading from the file
    handle <- openFile "2025_03_input" ReadMode
    contents <- hGetContents handle
    let lins = lines contents
    
    let firstJolt    = map (\x -> maxJolt $ init x) lins
    let firstJoltIdx = zipWith findIdx firstJolt lins
    let secndOptions = zipWith drop (map (\x -> x+1) firstJoltIdx) lins
    let secondJolt   = map maxJolt secndOptions

    let joltage      = zipWith ((\x y -> read [x, y]) :: Char -> Char -> Int) firstJolt secondJolt
    let totalJoltage = sum joltage

    putStr $ show totalJoltage

pt2 = do
    -- Reading from the file
    handle <- openFile "2025_03_input" ReadMode
    contents <- hGetContents handle
    let lins = lines contents

    let totalJoltage = sum $ map newMaxJoltCombo lins

    putStr $ show totalJoltage
