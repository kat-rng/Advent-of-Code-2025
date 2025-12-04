import Text.Regex.Posix
import Data.List
import System.IO

main = do
    -- Reading from the file
    handle <- openFile "2024_02_input" ReadMode
    contents <- hGetContents handle
    let x = lines contents

    -- Find the integer list representation of each line
    putStr $ show totalSafe
