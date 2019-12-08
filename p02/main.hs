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
