import Data.List
import System.IO

-- Create a list of integers from a string line
lineToInts :: String -> [Integer]
lineToInts line =
    map read $ words line

main = do
    -- Reading from the file
    handle <- openFile "2024_01_input1" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    let matrix = map lineToInts x
