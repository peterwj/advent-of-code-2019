import Test.HUnit

replaceNth :: (Eq a, Show a) => Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx value (x:xs)
  | idx == 0 = value:xs
  | idx < 0 = error "invalid list index"
  | otherwise = x:(replaceNth (idx - 1) value xs)

testReplaceWith :: (Eq a, Show a) => [a] -> a -> Int -> [a] -> Test
testReplaceWith lst value idx expected = TestLabel "test" $ TestCase (assertEqual "" expected (replaceNth idx value lst))

applyOperator ip memory operator = let arg1Ptr = memory!!(ip+1) in
  let arg2Ptr = memory!!(ip+2) in
    let arg2 = memory!!(arg2Ptr) in
      let arg1 = memory!!(arg1Ptr) in
        let arithResult = operator arg1 arg2 in
          let resultPtr = memory!!(ip+3) in
            replaceNth resultPtr arithResult memory

testApplyOperator ip memory operator expectedmem = TestLabel "test" $ TestCase (assertEqual "" expectedmem (applyOperator ip memory operator))

runComputer :: Int -> [Int] -> [Int]
runComputer ip memory = let op = memory!!ip in
  if op == 99
  then memory
  else let operator = case op of
                            1 -> (+)
                            2 -> (*) in
         let newMemory = applyOperator ip memory operator in
           runComputer (ip+4) newMemory

reportResult :: [Int] -> Int
reportResult memory = (runComputer 0 memory)!!0

initalMemory :: [Int]
initalMemory = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0]

generateMemory noun verb = (head initalMemory):noun:verb:(drop 3 initalMemory)

targetResult :: Int
targetResult = 19690720

inputRange :: [Int]
inputRange = [0..99]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

guessInput :: [Int] -> Int -> (Int, Int)
guessInput memory desiredResult = head $ filter (
  \(noun, verb) -> reportResult (generateMemory noun verb) == desiredResult
                                         ) (cartProd inputRange inputRange)

testPart1 :: [Int] -> [Int] -> Test
testPart1 inputMemory expectedMemory = TestLabel "test" $ TestCase (assertEqual "" expectedMemory (runComputer 0 inputMemory))

tests = TestList [
  (testReplaceWith [1,2] 100 0 [100,2]),
  (testReplaceWith [1,2] 100 1 [1,100]),
  (testApplyOperator 0 [1,0,0,0] (+) [2,0,0,0]),
  (testPart1 [99] [99]),
  (testPart1 [1,0,0,0,99] [2,0,0,0,99]),
  (testPart1 [2,3,0,3,99] [2,3,0,6,99]),
  (testPart1 [1,9,10,3,2,3,11,0,99,30,40,50] [3500,9,10,70,2,3,11,0,99,30,40,50])
                 ]
