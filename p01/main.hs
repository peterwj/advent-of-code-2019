import Test.HUnit
import System.IO

main = do
       input <- openFile "part1-input.txt" ReadMode
       inpStr <- hGetContents input
       let result = processData inpStr
       putStr result

str2num :: String -> Int
str2num = read

fileContentsToMasses :: String -> [Int]
fileContentsToMasses ms = map str2num (lines ms)

processData :: String -> String
processData d = show (sum (map massToFuel (fileContentsToMasses d)))

-- Specifically, to find the fuel required for a module, take its mass, divide by three,
-- round down, and subtract 2.
massToFuel :: Int -> Int
massToFuel mass = floor (realToFrac mass / 3) - 2

test1 = TestCase (assertEqual "" 2 (massToFuel 12))
test2 = TestCase (assertEqual "" 2 (massToFuel 14))
test3 = TestCase (assertEqual "" 654 (massToFuel 1969))
test4 = TestCase (assertEqual "" 33583 (massToFuel 100756))
tests = TestList [
  TestLabel "test" test1,
  TestLabel "test" test2,
  TestLabel "test" test3,
  TestLabel "test" test4]
