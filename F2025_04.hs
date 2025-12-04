import Data.List
import System.IO

charToInt :: Char -> Int
charToInt c = case c of
    '.' -> 0
    '@' -> 1
    _   -> error "Invalid character"

pt1 = do
    -- Reading from the file
    handle <- openFile "2025_04_input" ReadMode
    contents <- hGetContents handle
    let lins = lines contents

pt2 = do
    Nothing
