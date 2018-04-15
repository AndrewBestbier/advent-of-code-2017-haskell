import Prelude 

integerToDigits :: Integer -> [Int]
integerToDigits n = map (\x -> read [x] :: Int) (show n)

question1 input = captcha digits 1
  where digits = integerToDigits input 

question2 input = captcha digits dropSize
  where digits = integerToDigits input
        dropSize = length digits `div` 2    

-- Shared helper function
captcha digits dropSize = sum [ x | (x,y) <- zippedList, x == y]
  where size = length digits 
        infiniteOriginal = cycle digits
        infiniteAdjusted = cycle $ drop dropSize infiniteOriginal
        zippedList = take size $ zip infiniteOriginal infiniteAdjusted