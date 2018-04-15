import Prelude 

integerToDigits :: Integer -> [Int]
integerToDigits n = map (\x -> read [x] :: Int) (show n)

summer (x:y:[]) 
  | x == y = x
  | otherwise = 0
summer (x:y:zs) 
  | x == y = x + summer (y:zs)
  | otherwise = summer (y:zs)

day1 input 
  | head digits == last digits = head digits + summer digits
  | otherwise = summer digits
    where digits = integerToDigits input
    
day2 input = sum [ x | (x,y) <- zippedList, x == y]
  where digits = integerToDigits input
        size = length digits 
        infiniteOriginal = cycle digits
        infiniteAdjusted = cycle $ drop (size `div` 2) infiniteOriginal
        zippedList = take size $ zip infiniteOriginal infiniteAdjusted
        