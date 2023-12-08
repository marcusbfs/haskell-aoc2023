module Day03 where

import Data.Char (isDigit)
import Data.HashMap.Strict qualified as HM
import Data.List (nub)

data Type = Value | Symbol deriving (Show, Eq)

data Item = Item {type_ :: Type, value_ :: Int, numDigs_ :: Int, sym_ :: String, mainPos_ :: Position} deriving (Show, Eq)

type Position = (Int, Int)

type GridItem = (Position, Item)

type Grid = HM.HashMap Position Item

data GridInfo = GridInfo {getGrid_ :: Grid, maxRow_ :: Int, maxCol_ :: Int} deriving (Show)

type ParseLineAnnotation = (Int, Int, Grid, String)

type ParseInputGoAnnotation = (Int, Int, Grid, [String])

-- Parses a single line.
parseLine :: ParseLineAnnotation -> ParseLineAnnotation
parseLine input@(_, _, _, "") = input
parseLine (row, col, grid, '.' : rest) = parseLine (row, col + 1, grid, rest)
parseLine (row, col, grid, input@(s : rest))
  | isDigit s = parseLine (row, col + length digits, addValueToGrid ((row, col), read digits) grid, remaining)
  -- is symbol
  | otherwise = parseLine (row, col + 1, addSymToGrid ((row, col), [s]) grid, rest)
  where
    digits = takeWhile isDigit input
    remaining = dropWhile isDigit input

-- Parses the raw input string into a structured grid.
parseInput :: String -> GridInfo
parseInput raw = GridInfo gf (length inputLines - 1) (length (head inputLines) - 1)
  where
    inputLines = lines raw
    (_, _, gf, _) = go (0, 0, HM.empty, inputLines)
    go :: ParseInputGoAnnotation -> ParseInputGoAnnotation
    go input@(_, _, _, []) = input
    go (r_, c_, g_, sh : st) = go (r_ + 1, 0, gf_, st)
      where
        (_, _, gf_, _) = parseLine (r_, c_, g_, sh)

-- Add a symbol to the existing grid.
addSymToGrid :: (Position, String) -> Grid -> Grid
addSymToGrid (pos, sym) = HM.insert pos (Item Symbol 0 0 sym pos)

-- Add a value to the existing grid. It keeps track of all positions that this value occupies.
addValueToGrid :: (Position, Int) -> Grid -> Grid
addValueToGrid ((x, y), val) = go [(x, ys) | ys <- [y .. y - 1 + numDigs_ vi]]
  where
    vi = Item Value val (length $ show val) "" (x, y)
    go :: [Position] -> Grid -> Grid
    go [] g_ = g_
    go (p : ps) g_ = go ps (HM.insert p vi g_)

-- Giving a position and the maximum boundaries of the grid, return the adjecent positions
adjPos :: Position -> Position -> [Position]
adjPos (x, y) (maxX, maxY) = filter (\(xx, yy) -> not (xx == x && yy == y) && (xx >= 0 && xx <= maxX) && (yy >= 0 && yy <= maxY)) possibilities
  where
    possibilities = [(x + xx, y + yy) | xx <- [-1, 0, 1], yy <- [-1, 0, 1]]

solvePart1 :: GridInfo -> Int
solvePart1 (GridInfo grid maxRow maxCol) = foldl (\acc i -> value_ i + acc) 0 allPotentianValues
  where
    symbols = filter (\(_, item) -> type_ item == Symbol) $ HM.toList grid
    allPotentianValues = concatMap findValuesAdjToSymbol symbols

    findValuesAdjToSymbol :: GridItem -> [Item]
    findValuesAdjToSymbol ((sx, sy), _) = nub $ map snd potentialValues
      where
        poss = adjPos (sx, sy) (maxRow, maxCol)
        potentialValues = filter (\(pos, i_) -> pos `elem` poss && type_ i_ == Value) $ HM.toList grid

-- Part 1

solve1 :: String -> String
solve1 = show . solvePart1 . parseInput

-- Part 2

-- TODO: refactor so it uses the shared functionalities with part1
solvePart2 :: GridInfo -> Int
solvePart2 (GridInfo grid maxRow maxCol) = sum allGearRatios
  where
    -- Use only the gear
    symbols = filter (\(_, item) -> type_ item == Symbol && sym_ item == "*") $ HM.toList grid
    allGearRatios = map (\(i1 : i2 : _) -> value_ i1 * value_ i2) $ filter (\arr -> length arr == 2) $ map findValuesAdjToSymbol symbols

    findValuesAdjToSymbol :: GridItem -> [Item]
    findValuesAdjToSymbol ((sx, sy), _) = nub $ map snd potentialValues
      where
        poss = adjPos (sx, sy) (maxRow, maxCol)
        potentialValues = filter (\(pos, i_) -> pos `elem` poss && type_ i_ == Value) $ HM.toList grid

solve2 :: String -> String
solve2 = show . solvePart2 . parseInput

-- Test data
input_ =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]
