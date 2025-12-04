module F2025_04 (pt1, pt2) where

import Prelude as P
import Data.Massiv.Array as A
import System.IO

charToInt :: Char -> Int
charToInt c = case c of
    '.' -> 0
    '@' -> 1
    _   -> error "Invalid character"

-- Converts a string representation of the input into a Massiv array
inputToArray :: String -> Array U Ix2 Int
inputToArray xs = A.fromLists' A.Seq intLists
    where intLists = P.map (P.map charToInt) (lines xs)

-- Create a stencil that finds the total neighbors that are paper rolls for each point
sum3x3Stencil :: Integral a => Stencil Ix2 a a
sum3x3Stencil = makeStencil (Sz (3 :. 3)) (1 :. 1) 
    (\get ->
         get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1)
      +  get ( 0 :. -1) +                 get ( 0 :. 1)
      +  get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)
      
    )
{-# INLINE sum3x3Stencil #-}

-- Creating a padding to ensure the output from the stencil operation 
-- will be the same dimensions as the input
boxPad0 :: Padding Ix2 Int
boxPad0 = Padding (Sz2 1 1) (Sz2 1 1) (Fill 0)

isAccesible :: Array U Ix2 Int -> Array U Ix2 Int
isAccesible a = do
    -- Pad the array
    let padA = applyStencil boxPad0 sum3x3Stencil a

    -- Determine accessibility
    let isAccess = A.map (\x -> fromEnum (x<4)) (A.computeAs U padA)
    A.computeAs U isAccess

-- recursively count reachable paper rolls and remove them from the map 
-- until no new reachable rolls are discovered
totalReachable :: Int -> Array U Ix2 Int -> Int
totalReachable n a = case newReachable of
    0 -> n
    _ -> totalReachable (n+newReachable) reachableRemoved
    where 
        isReachable = A.zipWith (*) a (isAccesible a)
        newReachable  = A.sum isReachable
        reachableRemoved  = A.computeAs U $ A.zipWith (-) a isReachable

totalReachableTest n a = case newReachable of
    0 -> reachableRemoved
    _ -> reachableRemoved
    where 
        isReachable = isAccesible a
        newReachable  = A.sum isRealReachable
        isRealReachable = A.zipWith (*) isReachable a
        reachableRemoved  = A.computeAs U $ A.zipWith (-) a isRealReachable

newTotalReachable :: Array U Ix2 Int -> Int
newTotalReachable = totalReachable 0

pt1 :: IO ()
pt1 = do
    -- Reading from the file
    handle <- openFile "2025_04_input" ReadMode
    contents <- hGetContents handle

    -- Get the list contents
    let arr = inputToArray contents

    --Find the total accessible paper rolls with NO removals
    let totalAcc =  A.sum $ A.zipWith (*) (isAccesible arr) arr 
    putStr $ show totalAcc

pt2 :: IO ()
pt2 = do
    -- Reading from the file
    handle <- openFile "2025_04_input" ReadMode
    contents <- hGetContents handle

    -- Get the list contents
    let arr = inputToArray contents

    -- Find the total accesible paper rolls with removals
    let totalAcc = totalReachable 0 arr

    putStr $ show totalAcc

