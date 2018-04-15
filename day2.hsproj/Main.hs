module Main where
  
import System.Environment
import System.IO

main 
  = do
    { ["--file", fname] <- getArgs
    ; input <- readFile fname
    ; putStr . show $ question2 input }

question1 input = sum $ map minMaxDifference $ map words $ lines input 

minMaxDifference input = maximum inputToInts - minimum inputToInts
  where inputToInts =  map read input :: [Int]
        
question2 input = sum $ map evenlyDivisibleDifference $ map words $ lines input 

evenlyDivisibleDifference input = [ x `div` y | x <- inputToInts, y <- inputToInts, x /= y, x `mod` y == 0] !! 0
  where inputToInts =  map read input :: [Int]