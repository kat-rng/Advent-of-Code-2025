import Data.List
import System.IO

-- Take the sum of the absolute differences
totalDiff :: [[Integer]] -> Integer
totalDiff x = sum (map abs (zipWith (-) (sort (head x)) (sort (last x))))

-- Create a list of two integers from a string line
lineToInts :: String -> [Integer]
lineToInts line =
    map readAsInt (words line)

-- Read the string as integers
readAsInt :: String -> Integer
readAsInt x = read x

-- Compare two lists
main = do
    -- Reading from the file
    handle <- openFile "2024_01_input1" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    let matrix = map lineToInts x

    -- Take the differences of the transposed matrix
    let result = totalDiff (transpose matrix)
    putStr (show result)
