module Day06 where

import Utils (getNumsFromString)

data Race = Race {time_ :: Int, dist_ :: Int} deriving (Show)

parseInput :: String -> [Race]
parseInput raw = zipWith Race ts ds
  where
    (tsStr : dsStr : _) = lines raw
    ts = getNumsFromString (drop 5 tsStr) ' '
    ds = getNumsFromString (drop 9 dsStr) ' '

computeTotalDistance :: Int -> Int -> Int
computeTotalDistance raceTime holdTime = holdTime * (raceTime - holdTime)

getWinningConditions :: Race -> [Int]
getWinningConditions (Race time dist) = filter (> dist) $ map (computeTotalDistance time) [1 .. time - 1]

-- Solve 1
solve1 :: String -> String
solve1 = show . product . map (length . getWinningConditions) . parseInput

-- Solve 2
-- Just ignore the spaces.
solve2 :: String -> String
solve2 = solve1 . filter (/= ' ')

-- Test data

input_ =
  [ "Time:      7  15   30",
    "Distance:  9  40  200"
  ]

raw_ = unlines input_
