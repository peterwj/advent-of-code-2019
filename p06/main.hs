module Day06 where

import Data.List.Split
import Data.Map
import Data.Maybe
import Data.Tree

type Object = String

data ParsedOrbit = ParsedOrbit { poSatellite :: Object, poCenter :: Object} deriving Show

type OrbitTree = Tree Object

parseLine :: String -> ParsedOrbit
parseLine line = let parts = splitOn ")" line in
  ParsedOrbit (parts!!1) (head parts)

buildOrbitMapFoldFn :: Map Object [Object] -> ParsedOrbit -> Map Object [Object]
buildOrbitMapFoldFn result orbit = let key = poCenter orbit in
  insertWith (
    \newVal oldVal -> head newVal:oldVal
  ) key [poSatellite orbit] result

-- builds mapping from centers to list of their satellites
buildOrbitMap :: [ParsedOrbit] -> Map Object [Object]
buildOrbitMap = Prelude.foldl buildOrbitMapFoldFn empty

buildOrbitTreeRec :: Map Object [Object] -> Object -> OrbitTree
buildOrbitTreeRec orbitMap object = let val = Data.Map.lookup object orbitMap in
  if isNothing val
  then Node object []
  else Node object $ Prelude.map (buildOrbitTreeRec orbitMap) (fromJust val)

buildOrbitTree :: Map Object [Object] -> OrbitTree
buildOrbitTree orbitMap = buildOrbitTreeRec orbitMap "COM"


--foldFn needle acc tree = let recResult = findDepthInTree (Just $ 1 + fromJust acc) tree needle in
--  if isNothing recResult then Nothing
--  else error "asdf"
--
--findDepthInTree :: Maybe Int -> OrbitTree -> Object -> Maybe Int
--findDepthInTree depthSoFar tree needle = if rootLabel tree == needle
--  then Just 0
--  else if subForest tree == []
--  then Nothing
--  else Prelude.foldl (
--  foldFn needle
--  ) depthSoFar (subForest tree)

foldFn :: Object -> [Maybe Int] -> Maybe Int
foldFn = error "asdf"

computeParentOrbits :: OrbitTree -> Object -> Maybe Int
--computeParentOrbits = findDepthInTree $ Just 0
computeParentOrbits = foldTree foldFn

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
