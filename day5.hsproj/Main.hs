module Main where
  
import System.Environment
import System.IO
import Data.List
import Data.Sequence as Seq

main 
  = do
    { ["--file", fname] <- getArgs
    ; input <- readFile fname
    ; putStr . show $ question2 input }
    
question1 input = listToStep $ map stringToInt $ lines input
  where stringToInt input = read input :: Int
  
listToStep input = stepper (fromList input) 0 0
 
stepper input position step
  | newPosition > ((Seq.length input) - 1) = step + 1
  | otherwise = stepper newInput newPosition (step + 1)
      where positionValue = index input position
            newPosition = position + positionValue
            newInput = update position (positionValue + 1) input
  


question2 input = listToStep2 $ map stringToInt $ lines input
  where stringToInt input = read input :: Int
  

listToStep2 input = stepper2 (fromList input) 0 0
 
stepper2 input position step
  | newPosition > ((Seq.length input) - 1) = step + 1
  | positionValue >= 3 = stepper2 newInputDecreased newPosition (step + 1)
  | otherwise = stepper2 newInput newPosition (step + 1)
      where positionValue = index input position
            newPosition = position + positionValue
            newInput = update position (positionValue + 1) input
            newInputDecreased = update position (positionValue - 1) input