module Main where
  
import System.Environment
import System.IO

main 
  = do
    { ["--file", fname] <- getArgs
    ; input <- readFile fname
    ; putStr . show $ question1 input }

question1 input = sum $ map minMaxDifference $ map words $ lines input 

minMaxDifference input = maximum inputToInts - minimum inputToInts
  where inputToInts =  map read input :: [Int]
        