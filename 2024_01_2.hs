import Data.List
import System.IO

-- Create a list of two integers from a string line
lineToInts :: String -> [Integer]
lineToInts line =
    map read (words line)

-- Find total number of matches
countMatches :: [Integer] -> Integer -> Integer
countMatches target param = toInteger $ length $ filter (==param) target

-- Calculate the similarity score
findSimilarity :: [Integer] -> Integer -> Integer
findSimilarity target param = param * countMatches target param


-- Compare two lists
main = do
    -- Reading from the file
    handle <- openFile "2024_01_input1" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    let matrix = transpose (map lineToInts x)
    let l1 = head matrix
    let l2 = last matrix

    -- Curry the target list
    let similar = findSimilarity l2

    -- Find the total similarity
    let simScore = sum (map similar l1)

    putStr (show simScore)
