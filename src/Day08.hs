module Day08 (solve1, solve2) where

import Data.HashMap.Strict qualified as HM

data Direction = DLeft | DRight deriving (Show, Eq)

type NetworkType = HM.HashMap String (String, String)

data Map = Map
  { instructions_ :: [Direction],
    network_ :: NetworkType
  }
  deriving (Show)

parseInput :: String -> Map
parseInput raw =
  Map
    (map (\c -> if c == 'L' then DLeft else DRight) dirs)
    (HM.fromList $ map parseLine maps)
  where
    (dirs : _ : maps) = lines raw

    parseLine :: String -> (String, (String, String))
    parseLine line = (key, (l, r))
      where
        key = take 3 line
        l = take 3 $ drop 1 $ dropWhile (/= '(') line
        r = take 3 $ drop 2 $ dropWhile (/= ',') line

-- Given the map and the starting locations, follow the instructions.
applyInstructions :: Map -> [String] -> [String]
applyInstructions (Map [] _) starting = starting
applyInstructions (Map (i : is) m) starting = applyInstructions (Map is m) (applySingleInstruction starting i)
  where
    applySingleInstruction :: [String] -> Direction -> [String]
    applySingleInstruction i_ DLeft = map (\s -> fst $ HM.lookupDefault undefined s m) i_
    applySingleInstruction i_ DRight = map (\s -> snd $ HM.lookupDefault undefined s m) i_

-- Part 1

solveBruteForce :: [String] -> String -> String
solveBruteForce startingPoints raw = show $ go 0 startingPoints * length (instructions_ p)
  where
    p = parseInput raw
    go :: Int -> [String] -> Int
    go count loc
      | all ((== 'Z') . last) loc = count
      | otherwise = go (count + 1) (applyInstructions p loc)

solve1 :: String -> String
solve1 = solveBruteForce ["AAA"]

-- Part 2

solve2BruteForce :: String -> String
solve2BruteForce raw = solveBruteForce allStartingPoints raw
  where
    p = parseInput raw
    allStartingPoints = filter ((== 'A') . last) $ HM.keys $ network_ p

-- Brute force here just doesn't cut it.
-- We can take advantage on the fact that all paths starting with "**A" goes into
-- a cycle. We can count the steps in each cycle and the minimum factor of all of them.
-- Haskell has LCM that does this.

countCycleSteps :: String -> Map -> Int
countCycleSteps start map_ = length (instructions_ map_) * go 0 start
  where
    go count loc
      | count == 0 || (last loc /= 'Z') = go (count + 1) (head $ applyInstructions map_ [loc])
      | otherwise = count

solve2 :: String -> String
solve2 raw = show $ foldl1 lcm $ map (`countCycleSteps` p) allStartingPoints
  where
    p = parseInput raw
    allStartingPoints = filter ((== 'A') . last) $ HM.keys $ network_ p

-- Test

input_ :: [String]
input_ =
  [ "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
  ]

raw_ = unlines input_

input2_ =
  [ "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  ]

raw2_ = unlines input2_

input3_ =
  [ "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  ]
