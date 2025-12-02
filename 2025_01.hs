import Data.List
import System.IO

lineToRotation :: String -> Int
lineToRotation line = do
    let dir = strToDirection $ head line
    let mag = read $ tail line
    dir * mag

strToDirection :: Char -> Int
strToDirection id = case id of
    'R' ->  1
    'L' -> -1
    _   -> error "Bad direction ID"

pt1 = do
    -- Reading from the file
    handle <- openFile "2025_01_input" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    let deltas = map lineToRotation x

    -- Find the cumulative sum (starting from 50)
    let cumsum = scanl1 (+) (50 : deltas)

    -- Find the final location for each step on the dial
    let locs   = map (\x -> x `mod` 100) cumsum

    -- Find the number of times said location is 0 
    let total0 = length $ filter (== 0) locs

    putStr $ show total0
