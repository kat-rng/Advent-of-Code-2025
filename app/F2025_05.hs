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
    (ranges, ids)

-- Read the two elements of the ranges by splitting on dashes
readRanges :: String -> [Int]
readRanges xs = map read $ splitOn "-" xs

-- Checks how many ranges the int falls within
withinWhichBounds :: [[Int]] -> Int -> Int
withinWhichBounds ranges target = sum $ map (fromEnum . withinBounds target) ranges

-- Checks if the integer falls within the target range
withinBounds ::  Int -> [Int] -> Bool
withinBounds target range = target <= upper && target >= lower
    where
        upper = last range
        lower = head range

-- checks if the target range overlaps with the bounding range
boundWithinBound ::  [Int] -> [Int] -> Bool
boundWithinBound boundRange targetRange = do
    any (`withinBounds` boundRange) targetRange

-- Checks how many bounds are satisfied by each id
allBounds :: ([[Int]], [Int]) -> [Int]
allBounds (ranges, ids) = do
    let boundCheck = withinWhichBounds ranges
    map boundCheck ids

-- Counts the number of ids within bounds
sumWithinBounds :: ([[Int]], [Int]) -> Int
sumWithinBounds rangeIds = do
    let boundedCounts = allBounds rangeIds
    sum $ map (\x -> fromEnum (x>0)) boundedCounts

-- Find the maximum upper bound and minimum lower bound
maxExtent :: [[Int]] -> [Int]
maxExtent overlaps = do
    [minimum $ head overlapCols, maximum $ last overlapCols]
    where
        overlapCols = transpose overlaps

-- Splits the list of ranges in two ranges
-- then appends the maximum extent of the overlaps
mergeOverlaps :: ([[Int]], [Int]) -> [[Int]]
mergeOverlaps (ranges, targetRange) = do
    let (nonOverlap, overlap) = partition (boundWithinBound targetRange) ranges
    nonOverlap ++ [maxExtent overlap]

-- merge until the length of the bounds list doesn't change
mergeUntilStable :: Int -> [[Int]] -> [[Int]]
mergeUntilStable n ranges 
    | n == boundLength  = ranges
    | otherwise         = mergeUntilStable boundLength nextBounds
    where 
        nextBounds  = mergeOverlaps (ranges, head ranges)
        boundLength = length nextBounds

-- finds the size of a range (inclusive)
boundSize :: [Int] -> Int
boundSize range = last range - head range + 1

totalBounds :: [[Int]] -> Int
totalBounds ranges = sum $ map boundSize $ mergeUntilStable 0 ranges

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
    -- Reading from the file
    handle <- openFile "2025_05_input" ReadMode
    contents <- hGetContents handle

    -- Get the list contents
    let (ranges, ids) = readInput contents
    putStr $ show $ totalBounds ranges

