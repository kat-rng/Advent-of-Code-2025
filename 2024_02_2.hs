import Data.List
import System.IO

-- Create a list of two integers from a string line
lineToInts :: String -> [Integer]
lineToInts line =
    map readAsInt (words line)

-- Read the string as integers
readAsInt :: String -> Integer
readAsInt x = read x

-- Find total number of matches
countMatches :: ([Integer], Integer) -> Integer
countMatches (list, target) = length (filter (==target) list)


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

    putStr (show result)
