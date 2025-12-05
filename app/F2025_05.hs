module F2025_05 (pt1, pt2) where

import System.IO
import Data.List.Split
import Data.List
import Text.ParserCombinators.ReadP (satisfy)

-- Reads the ranges and ids 
-- This assumes the blank line between them has been removed
readInput :: String -> ([[Int]], [Int])
readInput xs = do
    -- Grab the lines and store them in a list of strings
    let lins = lines xs

    -- Grab the ranges by identifying that they have dashes in them
    let ranges = map readRanges $ filter (\x -> '-' `elem`    x) lins

    -- Do the reverse to find ids
    let ids    = map read       $ filter (\x -> '-' `notElem` x) lins

    -- Return both with the ranges transposed for easier lookup
    (transpose ranges, ids)

-- Read the two elements of the ranges by splitting on dashes
readRanges :: String -> [Int]
readRanges xs = map read $ splitOn "-" xs

-- Checks how many ranges the int falls within
withinBounds :: [[Int]] -> Int -> Int
withinBounds ranges target = do
    let satisfiedUp  = map (<= target) upper
    let satisfiedLow = map (>= target) lower
    sum $ map fromEnum $ zipWith (&&) satisfiedUp satisfiedLow
    where 
        upper = head ranges
        lower = last ranges

-- Checks how many bounds are satisfied by each id
allBounds :: ([[Int]], [Int]) -> [Int]
allBounds (ranges, ids) = do
    let boundCheck = withinBounds ranges
    map boundCheck ids

-- Counts the number of ids within bounds
sumWithinBounds :: ([[Int]], [Int]) -> Int
sumWithinBounds rangeIds = do
    let boundedCounts = allBounds rangeIds
    sum $ map (\x -> fromEnum (x>0)) boundedCounts

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_05_input" ReadMode
    contents <- hGetContents handle

    -- Get the list contents
    let totalFresh = sumWithinBounds $ readInput contents

    --Find the total accessible paper rolls with NO removals
    putStr $ show totalFresh

pt2 :: IO ()
pt2 = do
    putStr "Nothing"

