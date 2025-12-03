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

-- drops everything up to the first occurence of x
dropIdx :: Char -> String -> String
dropIdx x xs = case i of 
    Just i -> drop (i+1) xs
    Nothing -> error "Jolt value missing" 
    where i = elemIndex x xs





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
    Nothing
