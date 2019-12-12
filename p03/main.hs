import Data.List
import Data.List.Ordered
import Data.List.Split
import Data.Map
import System.IO
import Test.HUnit

data Direction = U | D | L | R deriving Show
type Operation = (Direction, Int)
data Coord = Coord { cx :: Int, cy :: Int } deriving (Show, Eq, Ord)
type PathState = (CoordMap, Int) -- CoordMap plus steps so far
type CoordMap = Map Coord Int -- maps coordinate to number of steps to arrive there

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
  | otherwise = takeWhile (>= end) [start,(start - 1)..end]

distanceBetween :: Coord -> Coord -> Int
distanceBetween c1 c2 = abs (cx c1 - cx c2) + abs (cy c1 - cy c2)

pathUpdateFoldFn :: PathState -> Coord -> PathState
pathUpdateFoldFn (currCoordMap, distanceSoFar) coord = let newDistnace = distanceSoFar + 1 in
  (Data.Map.insert coord newDistnace currCoordMap, newDistnace)

-- (
--              \(currPathMap, distanceSoFar) coord -> (Data.Map.insert coord (distanceSoFar + 1) currPathMap, distanceSoFar + 1)
--              )

generateWireCoordsRec :: PathState -> Coord -> [Operation] -> PathState
generateWireCoordsRec path _ [] = path
generateWireCoordsRec pathState current (op:ops) = let newPos = move current op in
  let xs = genRange (cx current) (cx newPos) in
    let ys = genRange (cy current) (cy newPos) in
      let intermediateCoords = [Coord x y | x <- xs, y <- ys] in
        -- drop the first member of intermediateCoords since it's the same as the last
        -- member in the last recursive call.
        let updatedPath = Data.List.foldl pathUpdateFoldFn pathState (tail intermediateCoords)
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

generateWireCoords :: [Operation] -> CoordMap
generateWireCoords ops = fst (generateWireCoordsRec (empty, 0) (Coord 0 0) ops)

findIntersections :: [Operation] -> [Operation] -> CoordMap
findIntersections ops1 ops2 = intersectionWith (+) (generateWireCoords ops1) (generateWireCoords ops2)

findClosestCrossFoldFn :: (Coord, Int) -> Coord -> Int -> (Coord, Int)
findClosestCrossFoldFn (bestCoord, bestDistance) coord distance = if distance < bestDistance
                      then (coord, distance)
                      else (bestCoord, bestDistance)

sentinelCoord = (Coord 0 0, 100000000)

findClosestCross :: [Operation] -> [Operation] -> Int
findClosestCross ops1 ops2 = snd $ foldlWithKey findClosestCrossFoldFn sentinelCoord (findIntersections ops1 ops2)

testFindClosestCross ops1 ops2 expectedResult =
  TestCase (assertEqual "" expectedResult (findClosestCross ops1 ops2))

processData :: String -> String
processData d = let wireData = lines d in
  let wire1Ops = parseWireInstructions $ head wireData in
    let wire2Ops = parseWireInstructions $ wireData!!1 in
      show $ findClosestCross wire1Ops wire2Ops

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
                         ) 40,
  testFindClosestCross (
      parseWireInstructions "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                       ) (
      parseWireInstructions "U62,R66,U55,R34,D71,R55,D58,R83"
                         ) 610,
  testFindClosestCross (
      parseWireInstructions "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                       ) (
      parseWireInstructions "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
                         ) 410
  ]
