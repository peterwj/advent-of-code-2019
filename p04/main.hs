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
digitsIncreaseHelper (d1, d2) = char2Int d1 <= char2Int d2

digitsIncrease :: Password -> Bool
digitsIncrease password = let pairs = adjacentDigits password in
  all digitsIncreaseHelper pairs

twoAdjacentNumbersSame :: Password -> Bool
twoAdjacentNumbersSame password = let pairs = adjacentDigits password in
    any (uncurry ( == )) pairs

groupFoldFn :: [String] -> Char -> [String]
groupFoldFn result c
  | null result = [[c]]
  | otherwise = let lastGroup = last result in
      let lastGroupsChar = head lastGroup in
        if c == lastGroupsChar
        then init result ++ [lastGroup ++ [c]]
        else result ++ [[c]]

groupDigits :: Password -> [String]
groupDigits password = foldl groupFoldFn [] $ show password

groupsOfTwo :: Password -> Bool
groupsOfTwo password = let groups = groupDigits password in
  any (\group -> 2 == length group) groups

checkPasswordPart1 :: Password -> Bool
checkPasswordPart1 password = twoAdjacentNumbersSame password && digitsIncrease password

checkPasswordPart2 :: Password -> Bool
checkPasswordPart2 password = digitsIncrease password && groupsOfTwo password

solvePuzzle :: (Password -> Bool) -> Int
solvePuzzle checkPasswordFn = length $ filter checkPasswordFn puzzleInput
