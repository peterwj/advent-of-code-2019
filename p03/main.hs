import Data.List
import Data.List.Ordered
import Data.List.Split
import Data.Map
import System.IO
import Test.HUnit

data Direction = U | D | L | R deriving Show
type Operation = (Direction, Int)
data Coord = Coord { cx :: Int, cy :: Int } deriving (Show, Eq, Ord)
type CoordMap = Map Coord Bool

str2Direction :: Char -> Direction
str2Direction s = case s of
                    'U' -> U
                    'D' -> D
                    'L' -> L
                    'R' -> R

parseInstruction :: String -> Operation
parseInstruction ins = let steps =
                             read $ tail ins
                       in (str2Direction $ head ins, steps)

parseWireInstructions :: String -> [Operation]
parseWireInstructions line = Data.List.map parseInstruction $ splitOn "," line

genRange :: Int -> Int -> [Int]
genRange start end
  | start <= end = [start..end]
  | otherwise = [end..start]

generateWireCoordsRec :: CoordMap -> Coord -> [Operation] -> CoordMap
generateWireCoordsRec path _ [] = path
generateWireCoordsRec path current (op:ops) = let newPos = move current op in
  let xs = genRange (cx current) (cx newPos) in
    let ys = genRange (cy current) (cy newPos) in
      let intermediateCoords = [Coord x y | x <- xs, y <- ys] in
        -- drop the first member of intermediateCoords since it's the same as the last
        -- member in the last recursive call.
        let updatedPath = Data.List.foldl (
              \currPath coord -> Data.Map.insert coord True currPath
              ) path (tail intermediateCoords)
        in
          generateWireCoordsRec updatedPath newPos ops

move :: Coord -> Operation -> Coord
move current op = let stepSize = snd op in
  case fst op of
    U -> Coord (cx current) (cy current + stepSize)
    D -> Coord (cx current) (cy current - stepSize)
    L -> Coord (cx current - stepSize) (cy current)
    R -> Coord (cx current + stepSize) (cy current)

testMove :: Coord -> Operation -> Coord -> Test
testMove current op expectedEnd =
  TestCase (assertEqual "" expectedEnd (move current op))

compareCoords :: Coord -> Coord -> Ordering
compareCoords c1 c2 = let score1 = scoreCoord c1 in
      let score2 = scoreCoord c2 in
        if score1 < score2
        then LT
        else if score2 > score1
        then GT
        else EQ

generateWireCoords :: [Operation] -> CoordMap
generateWireCoords = generateWireCoordsRec empty (Coord 0 0)

findIntersections :: [Operation] -> [Operation] -> CoordMap
findIntersections ops1 ops2 = intersection (generateWireCoords ops1) (generateWireCoords ops2)

findClosestCrossFoldFn :: Coord -> Coord -> Bool -> Coord
findClosestCrossFoldFn currMin keyCoord _ = if scoreCoord keyCoord < scoreCoord currMin
                      then keyCoord
                      else currMin

sentinelCoord = Coord 1000000000 1000000000

findClosestCross :: [Operation] -> [Operation] -> Coord
findClosestCross ops1 ops2 = foldlWithKey findClosestCrossFoldFn sentinelCoord (findIntersections ops1 ops2)

scoreCoord :: Coord -> Int
scoreCoord coord = abs (cx coord) + abs (cy coord)

testFindClosestCross ops1 ops2 expectedResult =
  TestCase (assertEqual "" expectedResult (scoreCoord $ findClosestCross ops1 ops2))

processData :: String -> String
processData d = let wireData = lines d in
  let wire1Ops = parseWireInstructions $ head wireData in
    let wire2Ops = parseWireInstructions $ wireData!!1 in
      show $ scoreCoord $ findClosestCross wire1Ops wire2Ops

main = do
       runTestTT tests
       input <- openFile "input.txt" ReadMode
       inpStr <- hGetContents input
       let result = processData inpStr
       putStr result
       putStr "\n"

tests = TestList [
  testMove (Coord 0 0) (D, 10) (Coord 0 (-10)),
  testMove (Coord 5 5) (L, 5) (Coord 0 5),
  testMove (Coord (-3) (-3)) (R, 10) (Coord 7 (-3)),
  testFindClosestCross (
      parseWireInstructions "R8,U5,L5,D3"
                       ) (
      parseWireInstructions "U7,R6,D4,L4"
                         ) 6,
  testFindClosestCross (
      parseWireInstructions "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                       ) (
      parseWireInstructions "U62,R66,U55,R34,D71,R55,D58,R83"
                         ) 159,
  testFindClosestCross (
      parseWireInstructions "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                       ) (
      parseWireInstructions "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                         ) 135
  ]
