module Day05 where

import Data.List.Split qualified as DLS
import Utils (getNumsFromString, splitStrAtSep)

data MapRanges = MapRanges {destStart_ :: Int, sourceStart_ :: Int, rangeLen_ :: Int} deriving (Show)

data Map = Map {mapName_ :: String, ranges_ :: [MapRanges]} deriving (Show)

data Problem = Problem {seeds_ :: [Int], maps_ :: [Map]} deriving (Show)

-- Custom function that returns a value given a key and a map, following the rules in Part 1
getValueFromMap :: Int -> Map -> Int
getValueFromMap key (Map _ ranges) = if null validRanges then key else key - ss + ds
  where
    validRanges =
      filter
        ( \(MapRanges _ ss_ rl) ->
            key >= ss_ && key <= (ss_ + rl - 1)
        )
        ranges -- there should be only one valid range
    (MapRanges ds ss _) = head validRanges

-- Custom function that returns a key given a value and a map.
getKeyFromMap :: Int -> Map -> Int
getKeyFromMap val (Map _ ranges) = if null validRanges then val else val - ds + ss
  where
    validRanges =
      filter
        ( \(MapRanges ds_ _ rl) ->
            val >= ds_ && val <= (ds_ + rl - 1)
        )
        ranges -- there should be only one valid range
    (MapRanges ds ss _) = head validRanges

-- Creates a map from the raw data
createMap :: [String] -> Map
createMap (name : dat) =
  Map
    name
    ( map
        ( \line ->
            let (ds : ss : rl : _) = map read $ splitStrAtSep line ' '
             in MapRanges ds ss rl
        )
        dat
    )

-- Parses the raw input into a managable problem description
parseInput :: String -> Problem
parseInput raw = Problem seeds (map createMap $ DLS.splitWhen null mapData)
  where
    (seedsStr : _ : mapData) = lines raw
    seeds = getNumsFromString (tail $ dropWhile (/= ':') seedsStr) ' '

-- Apply seed to the sequence of maps and get the final value
getFinalValue :: Int -> [Map] -> Int
getFinalValue = foldl getValueFromMap

-- Part 1
solve1 :: String -> String
solve1 raw = show $ minimum $ map (`getFinalValue` maps) seeds
  where
    (Problem seeds maps) = parseInput raw

-- Part 2
-- Takes ~30 min on a 5600x - 16GB RAM
solve2BruteForce :: String -> String
solve2BruteForce raw = show $ minimum $ map (`getFinalValue` maps) seeds
  where
    (Problem seeds' maps) = parseInput raw
    -- We need to treat each pair of seeds as a range description.
    seeds = concatMap (\(ss : sr : _) -> [ss .. (ss - 1 + sr)]) $ DLS.chunksOf 2 seeds'

solve2 :: String -> String
solve2 _ = "brute force takes some time..."

-- TODO: Approach the problem considering the ranges.
-- We can map a seed range to (potentially) multiple location ranges.

-- Test data

input_ =
  [ "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  ]

raw_ = unlines input_

simpleMap = Map "Test Simple Map" [MapRanges 50 98 2, MapRanges 52 50 48]
