module F2025_07 (pt1, pt2) where

import System.IO
import Data.List

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_07_input" ReadMode
    contents <- hGetContents handle
    putStr $ show contents

pt2 :: IO ()
pt2 = do
    putStr $ show "Nothing"
