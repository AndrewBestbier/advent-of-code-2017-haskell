module Main where
  
import System.Environment
import System.IO
import Data.List

main 
  = do
    { ["--file", fname] <- getArgs
    ; input <- readFile fname
    ; putStr . show $ question2 input }

question1 = length . filter (\x -> x == True) . map compareOriginalToUniqueLength . map words . lines

question2 = length . filter (\x -> x == True) . map compareSortedOriginalToUniqueLength . map words . lines

compareOriginalToUniqueLength input = sizeOriginal == sizeUnique
  where sizeOriginal = length input 
        sizeUnique = length $ nub input 
        
compareSortedOriginalToUniqueLength input = sizeSorted == sizeUnique
  where sortedInput = map sort input
        sizeSorted = length sortedInput
        sizeUnique = length $ nub sortedInput