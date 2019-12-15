module Day05 where

import Test.HUnit
import Text.Printf

data Op = Add | Multiply | Input | Output | Halt | JT | JF | LT | Eq deriving (Show, Eq)
data Mode = Immediate | Position deriving (Show, Eq)

data Modes = Modes { first :: Mode, second :: Mode, third :: Mode } deriving Show
data ComputerState = ComputerState {
  csMemory :: [Int],
  csOutput :: [Int],
  csIp :: Int,
  csInputBuffer :: [Int]
  } deriving (Eq, Show)

type ParsedOp = (Op, Modes)

replaceNth :: (Eq a, Show a) => Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx value (x:xs)
  | idx == 0 = value:xs
  | idx < 0 = error "invalid list index"
  | otherwise = x:(replaceNth (idx - 1) value xs)

testReplaceWith :: (Eq a, Show a) => [a] -> a -> Int -> [a] -> Test
testReplaceWith lst value idx expected = TestLabel "test" $ TestCase (assertEqual "" expected (replaceNth idx value lst))

extractArg :: Int -> [Int] -> Mode -> Int -> Int
extractArg ip memory mode paramNum
  | mode == Position = let ptr = memory!!(ip + paramNum) in
      memory!!ptr
  | mode == Immediate = memory!!(ip + paramNum)

-- picks out the first and second args
extractArgs :: Int -> [Int]  -> Modes -> (Int, Int)
extractArgs ip memory modes = let arg2 = extractArg ip memory (second modes) 2 in
  let arg1 = extractArg ip memory (first modes) 1 in
    (arg1, arg2)

storeResult :: Int -> [Int] -> Int -> [Int]
storeResult ip memory result =
  let resultPtr = memory!!(ip+3) in
    replaceNth resultPtr result memory

applyBoolOperator :: Int -> [Int] -> (Int -> Int -> Bool) -> Modes -> [Int]
applyBoolOperator ip memory operator modes =
  let (arg1, arg2) = extractArgs ip memory modes in
    let result = if operator arg1 arg2
                 then 1 else 0 in
      storeResult ip memory result

applyArithOperator :: Int -> [Int] -> (Int -> Int -> Int) -> Modes -> [Int]
applyArithOperator ip memory operator modes =
  let (arg1, arg2) = extractArgs ip memory modes in
    let result = operator arg1 arg2 in
      storeResult ip memory result

testApplyOperator ip memory operator expectedmem = TestLabel "test" $ TestCase (assertEqual "" expectedmem (applyArithOperator ip memory operator (Modes Position Position Position)))

computeJump :: ParsedOp -> ComputerState -> Int
computeJump (op, modes) state = let ip = csIp state in
  let memory = csMemory state in
    let (arg1, arg2)  = extractArgs ip memory modes in
      let predicate = case op of
            JT -> (/=) 0
            JF -> (==) 0 in
        if predicate arg1 then arg2 else ip+3

nextIp :: ParsedOp -> ComputerState -> Int
nextIp (op, modes) state = let ip = csIp state in
  case op of
    Add -> ip + 4
    Multiply -> ip + 4
    Input -> ip + 2
    Output -> ip + 2
    Halt -> ip
    JT -> computeJump (op, modes) state
    JF -> computeJump (op, modes) state
    Day05.LT -> ip + 4
    Day05.Eq -> ip + 4

parseOpcode :: Int -> Op
parseOpcode op = let singleDigitOp = op `mod` 100 in -- strips off mode bits
  case singleDigitOp of
    1 -> Add
    2 -> Multiply
    3 -> Input
    4 -> Output
    5 -> JT
    6 -> JF
    7 -> Day05.LT
    8 -> Eq
    99 -> Halt
    _ -> error $ "unknown op " ++ show op

code2Mode :: Char -> Mode
code2Mode code
  | code == '0' = Position
  | code == '1' = Immediate

leftPadOp :: Int -> String
leftPadOp = printf "%05d"

-- returns mode of first, second, and third param
parseMode :: Int -> Modes
parseMode op = let leftPaddedOp = leftPadOp op in
  Modes (code2Mode(leftPaddedOp!!2)) (code2Mode(leftPaddedOp!!1)) (code2Mode(leftPaddedOp!!0))

parseOp :: Int -> ParsedOp
parseOp op = (parseOpcode op, parseMode op)

-- applies op to memory, returning the new memory
applyOperator :: ParsedOp -> ComputerState -> ComputerState
applyOperator (opcode, modes) state = let memory = csMemory state in
  let ip = csIp state in
    let newIp = nextIp (opcode, modes) state in
      let output = csOutput state in
        let inputBuffer = csInputBuffer state in
          case opcode of
            Add -> ComputerState (applyArithOperator ip memory (+) modes) output newIp inputBuffer
            Multiply -> ComputerState (applyArithOperator ip memory (*) modes) output newIp inputBuffer
            Day05.LT -> ComputerState (applyBoolOperator ip memory (<) modes) output newIp inputBuffer
            Day05.Eq -> ComputerState (applyBoolOperator ip memory (==) modes) output newIp inputBuffer
            JT -> ComputerState memory output newIp inputBuffer
            JF -> ComputerState memory output newIp inputBuffer
            Input -> let destPtr = memory!!(ip+1) in
              ComputerState (replaceNth destPtr (head inputBuffer) memory) output newIp (tail inputBuffer)
            Output -> let arg = fst $ extractArgs ip memory modes in
              ComputerState memory (output ++ [arg]) newIp inputBuffer

runComputer :: ComputerState -> ComputerState
runComputer state =
  let ip = csIp state in
    let memory = csMemory state in
      let encodedOp = memory!!ip in
        let (opcode, modes) = parseOp encodedOp in
          if opcode == Halt
          then state
          else let newState = applyOperator (opcode, modes) state in
            runComputer newState

testPart1 :: [Int] -> ComputerState -> Test
testPart1 inputMemory expectedState = TestLabel "test" $ TestCase (
    assertEqual "" expectedState
    (runComputer (ComputerState inputMemory [] 0 [1]))
  )

testPart2 :: [Int] -> Int -> [Int] -> Test
testPart2 inputMem expectedInput expectedOutput = let state = ComputerState inputMem [] 0 [expectedInput] in
  TestLabel "test" $ TestCase (
    assertEqual "" expectedOutput (csOutput $ runComputer state)
  )

tests = TestList [
  (testReplaceWith [1,2] 100 0 [100,2]),
  (testReplaceWith [1,2] 100 1 [1,100]),
  (testApplyOperator 0 [1,0,0,0] (+) [2,0,0,0]),
  (testPart1 [99]         (ComputerState [99] [] 0 [1])),
  (testPart1 [1,0,0,0,99] (ComputerState [2,0,0,0,99] [] 4 [1])),
  (testPart1 [2,3,0,3,99] (ComputerState [2,3,0,6,99] [] 4 [1])),
  (testPart1 [1,9,10,3,2,3,11,0,99,30,40,50] (ComputerState [3500,9,10,70,2,3,11,0,99,30,40,50] [] 8 [1])),
  (testPart1 [1002,4,3,4,33] (ComputerState [1002,4,3,4,99] [] 4 [1] )),
  (testPart1 [3,0,4,0,99] (ComputerState [1,0,4,0,99] [1] 4 [])),
  (testPart2 [1105,1,4,99,4,5,99] 1 [5]),
  (testPart2 [1105,0,4,99,4,5,99] 1 []),
  (testPart2 [1106,0,4,99,4,5,99] 1 [5]),
  (testPart2 [1106,1,4,99,4,5,99] 1 []),
  (testPart2 [1107,1,2,5,104,-1,99] 1 [1]),
  (testPart2 [1107,2,1,5,104,-1,99] 1 [0]),
  (testPart2 [1108,1,1,5,104,-1,99] 1 [1]),
  (testPart2 [1108,2,1,5,104,-1,99] 1 [0]),
  (testPart2 [3,9,8,9,10,9,4,9,99,-1,8] 8 [1]),
  (testPart2 [3,9,8,9,10,9,4,9,99,-1,8] 7 [0]),
  (testPart2 [3,3,1107,-1,8,3,4,3,99] 5 [1]),
  (testPart2 [3,3,1107,-1,8,3,4,3,99] 9 [0]),
  (testPart2 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 0 [0]),
  (testPart2 [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] 1 [1]),
  (testPart2 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 0 [0]),
  (testPart2 [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] 1 [1]),
  (testPart2 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] (-4) [999]),
  (testPart2 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 8 [1000]),
  (testPart2 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] 10 [1001])
                 ]


main = do
       runTestTT tests
