import Data.List
import System.IO

-- Create a list of integers from a string line
lineToInts :: String -> [Int]
lineToInts line =
    map read $ words line

diffList :: [Int] -> [Int]
diffList l = zipWith (-) (init l) (tail l)

data ReactorDirection = Unknown
        | Increasing
        | Decreasing
    deriving (Eq, Show)

getState :: Int -> ReactorDirection
getState change
    | change >  0 = Increasing
    | change <  0 = Decreasing
    | otherwise   = undefined

checkState :: ReactorDirection -> [Int] -> Bool
checkState direction differences
    | length differences == 0                           = True
    | isUnsafeDiff $ head differences                   = False
    | direction == Unknown || direction == currentDir   = checkState currentDir (tail differences)
    | direction /= currentDir                           = False
    | otherwise                                         = undefined
    where currentDir = getState $ head differences

newCheckState :: [Int] -> Bool
newCheckState = checkState Unknown

checkAllRemovals :: [Int] -> Bool
checkAllRemovals differences = or $ map newCheckState (newRemovalList differences)

newRemovalList :: [Int] -> [[Int]]
newRemovalList differences = removalList [] differences (length differences -1)

removalList :: [[Int]] -> [Int] -> Int -> [[Int]]
removalList perms differences index
    | index == -1                   = perms
    | length perms == 0             = removalList [differences] differences index
    | otherwise                     = removalList (deleteNth differences index:perms) differences (index-1)

deleteNth :: [Int]-> Int -> [Int]
deleteNth differences index =
  let (before, after) = splitAt index  differences
  in before ++ drop 1 after

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
    let diffmatrix  = map diffList matrix
    let safetys     = map checkAllRemovals diffmatrix
    let totalSafe   = sum $ map fromEnum safetys
    putStr $ show totalSafe
