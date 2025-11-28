import Data.List
import System.IO

-- Create a list of integers from a string line
lineToInts :: String -> [Int]
lineToInts line =
    map read $ words line

--Find the differences between each state
diffList :: [Int] -> [Int]
diffList l = zipWith (-) (init l) (tail l)

-- Defines what direction the reactor is traveling in
data ReactorDirection = Unknown
        | Increasing
        | Decreasing
    deriving (Eq, Show)

-- Check if the state is increasing or decreasing
getState :: Int -> ReactorDirection
getState change
    | change >  0 = Increasing
    | change <  0 = Decreasing
    | otherwise   = undefined

-- Check if a state is safe
checkState :: ReactorDirection -> [Int] -> Bool
checkState direction differences
    | length differences == 0                           = True
    | isUnsafeDiff $ head differences                   = False
    | direction == Unknown || direction == currentDir   = checkState currentDir (tail differences)
    | direction /= currentDir                           = False
    | otherwise                                         = undefined
    where currentDir = getState $ head differences

-- Check if a list of differences is safe
newCheckState :: [Int] -> Bool
newCheckState = checkState Unknown

-- See if any removal option makes it work
checkAllRemovals :: [Int] -> Bool
checkAllRemovals observations = or $ map newCheckState (map diffList (newRemovalList observations))

-- Find all the removal options
newRemovalList :: [Int] -> [[Int]]
newRemovalList observations = removalList [] observations (length observations -1)

-- Create a list of every possible element removal
removalList :: [[Int]] -> [Int] -> Int -> [[Int]]
removalList perms observations index
    | index == -1                   = perms
    | length perms == 0             = removalList [observations] observations index
    | otherwise                     = removalList (deleteNth observations index:perms) observations (index-1)

-- Remove the nth observation
deleteNth :: [Int]-> Int -> [Int]
deleteNth observations index =
  let (before, after) = splitAt index  observations
  in before ++ drop 1 after

-- Check if the reactor difference is unsafe
isUnsafeDiff :: Int -> Bool
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
    let safetys     = map checkAllRemovals matrix
    let totalSafe   = sum $ map fromEnum safetys
    putStr $ show totalSafe
