import Data.List
import System.IO

-- Create a list of integers from a string line
lineToInts :: String -> [Integer]
lineToInts line =
    map read $ words line

diffList :: [Integer] -> [Integer]
diffList l = zipWith (-) (init l) (tail l)

data ReactorDirection = Unknown
        | Increasing
        | Decreasing
    deriving (Eq, Show)

getState :: Integer -> ReactorDirection
getState change
    | change >  0 = Increasing
    | change <  0 = Decreasing
    | otherwise   = undefined

checkState :: ReactorDirection -> [Integer] -> Bool
checkState direction differences
    | length differences == 0                           = True
    | isUnsafeDiff $ head differences                   = False
    | direction == Unknown || direction == currentDir   = checkState currentDir (tail differences)
    | direction /= currentDir                           = False
    | otherwise                                         = undefined
    where currentDir = getState $ head differences

newCheckState :: [Integer] -> Bool
newCheckState = checkState Unknown

isUnsafeDiff :: Integer -> Bool
isUnsafeDiff difference
    | difference >  3     = True
    | difference < -3     = True
    | difference == 0     = True
    | otherwise           = False

main = do
    -- Reading from the file
    handle <- openFile "2024_02_input" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    let matrix      = map lineToInts x
    let diffmatrix  = map diffList matrix
    let safetys     = map newCheckState diffmatrix
    let totalSafe   = sum $ map fromEnum safetys
    putStr $ show totalSafe
