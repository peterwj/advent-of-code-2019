type Password = Int
type DigitPair = (Char, Char)

puzzleInput :: [Password]
puzzleInput = [284639..748759]

adjacentDigits :: Password -> [DigitPair]
adjacentDigits password =  let passwordAsStr = show password in
  zip (init passwordAsStr) (tail passwordAsStr)

char2Int :: Char -> Int
char2Int c = read [c]

digitsIncreaseHelper :: DigitPair -> Bool
digitsIncreaseHelper (d1, d2) = (char2Int d1) <= (char2Int d2)

digitsIncrease :: Password -> Bool
digitsIncrease password = let pairs = adjacentDigits password in
  all (digitsIncreaseHelper) pairs

twoAdjacentNumbersSame :: Password -> Bool
twoAdjacentNumbersSame password = let pairs = adjacentDigits password in
    any (\(d1, d2) -> d1 == d2) pairs

checkPassword :: Password -> Bool
checkPassword password = twoAdjacentNumbersSame password && digitsIncrease password

solvePuzzle :: Int
solvePuzzle = length $ filter checkPassword puzzleInput
