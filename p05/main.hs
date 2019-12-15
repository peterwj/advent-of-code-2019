import Test.HUnit
import Text.Printf

data Op = Add | Multiply | Input | Output | Halt | JT | JF | LT | Eq deriving (Show, Eq)
data Mode = Immediate | Position deriving (Show, Eq)

data Modes = Modes { first :: Mode, second :: Mode, third :: Mode }
data ComputerState = ComputerState {
  csMemory :: [Int],
  csOutput :: [Int],
  csIp :: Int,
  csInputBuffer :: [Int]
  } deriving (Eq, Show)

type ParsedOp = (Op, Modes)

inputVal = 1

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

applyArithOperator :: Int -> [Int] -> (Int -> Int -> Int) -> Modes -> [Int]
applyArithOperator ip memory operator modes = let arg2 = extractArg ip memory (second modes) 2 in
  let arg1 = extractArg ip memory (first modes) 1 in
    let arithResult = operator arg1 arg2 in
      let resultPtr = memory!!(ip+3) in
        replaceNth resultPtr arithResult memory

testApplyOperator ip memory operator expectedmem = TestLabel "test" $ TestCase (assertEqual "" expectedmem (applyArithOperator ip memory operator (Modes Position Position Position)))

-- outputs the new IP for the current IP and current op
nextIp :: Int -> Op -> Int
nextIp ip op
  | op == Add = ip + 4
  | op == Multiply = ip + 4
  | op == Input = ip + 2
  | op == Output = ip + 2
  | op == Halt = ip + 1

parseOpcode :: Int -> Op
parseOpcode op = let singleDigitOp = op `mod` 100 in -- strips off mode bits
  case singleDigitOp of
    1 -> Add
    2 -> Multiply
    3 -> Input
    4 -> Output
    99 -> Halt
    otherwise -> error $ "unknown op " ++ show op

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
    let newIp = nextIp ip opcode in
      let output = csOutput state in
        let inputBuffer = csInputBuffer state in
          case opcode of
            Add -> ComputerState (applyArithOperator ip memory (+) modes) output newIp inputBuffer
            Multiply -> ComputerState (applyArithOperator ip memory (*) modes) output newIp inputBuffer
            Input -> let destPtr = memory!!(ip+1) in
              ComputerState (replaceNth destPtr inputVal memory) output newIp (tail inputBuffer)
            Output -> let ptr = memory!!(ip+1) in
              ComputerState memory (output ++ [memory!!ptr]) newIp inputBuffer

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
    assertEqual "" (csOutput $ runComputer state) expectedOutput
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
  (testPart1 [3,0,4,0,99] (ComputerState [1,0,4,0,99] [inputVal] 4 []))
                 ]


main = do
       runTestTT tests
