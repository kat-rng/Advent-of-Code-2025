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

main = do
    -- Reading from the file
    handle <- openFile "2025_01_input" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    let deltas = map lineToRotation x
    putStr $ show matrix
