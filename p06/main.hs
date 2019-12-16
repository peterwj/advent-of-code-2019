module Day06 where
import Data.List.Split
import Data.Map
import Data.Maybe

type Object = String

data ParsedOrbit = ParsedOrbit { poSatellite :: Object, poCenter :: Object} deriving Show

parseLine :: String -> ParsedOrbit
parseLine line = let parts = splitOn ")" line in
  ParsedOrbit (parts!!1) (head parts)

data OrbitNode = OrbitNode {
  center :: Object,
  satellites :: [OrbitNode]
  } deriving Show

buildOrbitMapFoldFn :: Map Object [Object] -> ParsedOrbit -> Map Object [Object]
buildOrbitMapFoldFn result orbit = let key = poCenter orbit in
  insertWith (
    \newVal oldVal -> head newVal:oldVal
  ) key [poSatellite orbit] result

-- builds mapping from centers to list of their satellites
buildOrbitMap :: [ParsedOrbit] -> Map Object [Object]
buildOrbitMap = Prelude.foldl buildOrbitMapFoldFn empty

buildOrbitTreeRec :: Map Object [Object] -> Object -> OrbitNode
buildOrbitTreeRec orbitMap object = let val = Data.Map.lookup object orbitMap in
  if isNothing val
  then OrbitNode object []
  else OrbitNode object $ Prelude.map (buildOrbitTreeRec orbitMap) (fromJust val)

buildOrbitTree :: Map Object [Object] -> OrbitNode
buildOrbitTree orbitMap = buildOrbitTreeRec orbitMap "COM"

smallInput = [
  "COM)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L"]

main = buildOrbitTree $ buildOrbitMap $ Prelude.map parseLine smallInput
