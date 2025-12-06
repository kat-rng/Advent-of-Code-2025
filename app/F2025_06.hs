module F2025_06 (pt1, pt2) where

import System.IO
import Data.List
import Text.Printf

-- read the input into a set of columns of numbers (ns) and operations (os)
readInput :: String -> ([[String]], String)
readInput s = do
    let xs = map words $ lines s
    let ns = init xs
    let os = map head $ last xs
    (transpose ns, os)

-- zip the lists into tuples of "math problems" with string ints
readTuples :: ([[String]], String) -> [([String], Char)]
readTuples (ns, os) = zip ns os

-- zip the lists into tuples of "math problems"
readIntegers :: ([[String]], String) -> [([Integer], Char)]
readIntegers (ns, os) = do
    let ns_int = map (map read) ns
    zip ns_int os

-- Apply the operation defined by the char on the integers
applyOperation :: ([Integer], Char) -> Integer
applyOperation (ints, op) = case op of
    '*' -> product  ints
    '+' -> sum      ints
    _   -> error "invalid operation"

transposeStrings :: ([String], Char) -> ([Integer], Char)
transposeStrings (strs, op) = do
    let padSize = maximum $ map length strs
    let padStrs = map (printf "%*s" padSize) strs
    let ints = map (read . head . words)$ transpose padStrs
    (ints, op)

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_06_input" ReadMode
    contents <- hGetContents handle

    -- Get the total fresh
    let input = readIntegers $ readInput contents
    let output = sum $ map applyOperation input
    -- print it
    putStr $ show output

pt2 :: IO ()
pt2 = do
    -- Reading from the file
    handle <- openFile "2025_06_input" ReadMode
    contents <- hGetContents handle

    let input = map transposeStrings $ readTuples $ readInput contents
    let output = map applyOperation input

    putStr $ show input
