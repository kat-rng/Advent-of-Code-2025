module F2025_04 (pt1, pt2) where

import Data.List
import System.IO

charToInt :: Char -> Int
charToInt c = case c of
    '.' -> 0
    '@' -> 1
    _   -> error "Invalid character"

badFunction :: Char -> Int
badFunction a = a

-- Add this temporary function:
checkLinter x = (head x)

pt1 = do
    -- Reading from the file
    handle <- openFile "2025_04_input" ReadMode
    contents <- hGetContents handle
    let lins = lines contents

pt2 = do
    Nothing
