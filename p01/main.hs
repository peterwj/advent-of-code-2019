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
processData d = show (sum (map recMassToFuel (fileContentsToMasses d)))

-- Specifically, to find the fuel required for a module, take its mass, divide by three,
-- round down, and subtract 2.
massToFuel :: Int -> Int
massToFuel mass = floor (realToFrac mass / 3) - 2

recMassToFuel :: Int -> Int
recMassToFuel mass = let fuelForModule = massToFuel mass in
                       if fuelForModule <= 0
                       then 0
                       else fuelForModule + recMassToFuel fuelForModule

testPart fuelFn mass fuel = TestLabel "test" $ TestCase (assertEqual "" fuel (fuelFn mass))

testPart1 :: Int -> Int -> Test
testPart1 mass fuel = testPart massToFuel mass fuel

testPart2 :: Int -> Int -> Test
testPart2 mass fuel = testPart recMassToFuel mass fuel

tests = TestList [
  testPart1 12 2,
  testPart1 14 2,
  testPart1 1969 654,
  testPart1 100756 33583,
  testPart2 14 2,
  testPart2 1969 966,
  testPart2 100756 50346]
