import Prelude
import Data.Complex

data CompassPoint = E | N | W | S deriving (Show, Enum)
type Position = Complex Double
type Point = (Position, CompassPoint, Int)
type ComplexPoint = (Point, [Point])

-- Question 1
question1 n = (\((x:+y), _) -> abs x + abs y) . last . take n $ iterate step1 ((0:+0), E)

-- Question 2
question2 n = (\((_, _, x), _) -> x) $ last $ takeWhileInclusive (\((_, _, x), _) -> x < n) $ iterate step2 (((0:+0), E, 1), [])


-- Helpers

step1 :: (Position, CompassPoint) -> (Position, CompassPoint)
step1 ((0:+0), E) = (((1:+0), N))
step1 (oldPosition, compassPoint) = (move oldPosition compassPoint, maybeRotate oldPosition newPosition compassPoint)
  where newPosition = move oldPosition compassPoint

step2 :: ComplexPoint -> ComplexPoint
step2 (((0:+0), E, 1), []) = (((1:+0), N, 1), [((0:+0), E, 1)])
step2 ((oldPosition, compassPoint, value), acc) = ((move oldPosition compassPoint, maybeRotate oldPosition newPosition compassPoint, newValue), newAcc)
  where newPosition = move oldPosition compassPoint
        newAcc = acc ++ [(oldPosition, compassPoint, value)]
        newValue = findNewValue value newAcc newPosition

move :: Position -> CompassPoint -> Position
move (x :+ y) compassPoint = case compassPoint of
  N -> (x :+ (y + 1))
  E -> ((x + 1) :+ y)
  S -> (x :+ (y - 1))
  W -> ((x - 1) :+ y)

maybeRotate :: Position -> Position -> CompassPoint -> CompassPoint
maybeRotate (x1 :+ y1) (x2 :+ y2) compassPoint
  | x1 > 0 && y1 < 0 && x1 ==  abs y1 = turnLeft compassPoint
  | x2 == y2 = turnLeft compassPoint
  | abs x2 == y2 = turnLeft compassPoint
  | otherwise = compassPoint

turnLeft :: CompassPoint -> CompassPoint
turnLeft compassPoint = case compassPoint of
  N -> W
  W -> S
  S -> E
  E -> N

findNewValue :: Int -> [Point] -> Position -> Int
findNewValue oldValue acc newPosition = sum [z | ((x1:+y1), d, z) <- acc, (x2:+y2) <- surroundingPoints, x1 == x2, y1 == y2]
  where surroundingPoints = findSurroundingPoints newPosition

findSurroundingPoints :: Position -> [Position]
findSurroundingPoints (x :+ y) = [((x + 1) :+ y), ((x - 1) :+ y), (x :+ (y + 1)), (x :+ (y - 1)), ((x + 1) :+ (y + 1)), ((x - 1) :+ (y + 1)), ((x - 1) :+ (y - 1)), ((x + 1) :+ (y - 1))]


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
