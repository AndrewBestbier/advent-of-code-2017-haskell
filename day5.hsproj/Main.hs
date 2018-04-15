module Main where
  
import System.Environment
import System.IO
import Data.List
import Data.Sequence as Seq

main 
  = do
    { ["--file", fname] <- getArgs
    ; input <- readFile fname
    ; putStr . show $ question1 input }
    
question1 input = listToStep $ map stringToInt $ lines input
  where stringToInt input = read input :: Int
  
listToStep input = stepper (fromList input) 0 0
 
stepper input position step
  | newPosition > ((Seq.length input) - 1) = step + 1
  | otherwise = stepper newInput newPosition (step + 1)
      where positionValue = index input position
            newPosition = position + positionValue
            newInput = update position (positionValue + 1) input
  


