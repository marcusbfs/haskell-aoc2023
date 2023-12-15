module Day10 (solve1, solve2) where

import Data.HashMap.Strict qualified as HM
import Data.Maybe (isNothing)
import Debug.Trace (trace, traceShow, traceShowId)

data Direction = North | South | East | West deriving (Show)

type Position = (Int, Int)

type GridType = HM.HashMap Position Char

data Grid = Grid {sPos_ :: Position, grid_ :: GridType} deriving (Show)

parseInput :: String -> Grid
parseInput raw = Grid sPos grid_
  where
    ls = lines raw
    (_, grid_, _, sPos) = go (0, HM.empty, ls, (0, 0))

    parseLine :: (Int, Int, GridType, String, Position) -> (Int, Int, GridType, String, Position)
    parseLine g@(_, _, _, "", _) = g
    parseLine (row, col, grid, c : cs, pos) =
      parseLine
        ( row,
          col + 1,
          HM.insert
            (row, col)
            c
            grid,
          cs,
          if c == 'S' then (row, col) else pos
        )

    go :: (Int, GridType, [String], Position) -> (Int, GridType, [String], Position)
    go g@(_, _, [], _) = g
    go (row, grid, l : lss, pos) = go (row + 1, g, lss, pos')
      where
        (_, _, g, _, pos') = parseLine (row, 0, grid, l, pos)

-- Go to the indicated direction, trying to get the path to the starting position.
-- If the path leads to nowhere, return Nothing.
-- Else, it returns all the path positions.
traverseG :: Grid -> Direction -> Maybe [Position]
traverseG (Grid (px, py) grid) dir = case dir of
  North ->
    connect (px - 1, py) connectNorthOptions >>= \newPos ->
      case HM.lookup newPos grid of
        Just '|' -> (newPos :) <$> traverseG (Grid newPos grid) North
        Just 'F' -> (newPos :) <$> traverseG (Grid newPos grid) East
        Just '7' -> (newPos :) <$> traverseG (Grid newPos grid) West
        _ -> Just [newPos]
  South ->
    connect (px + 1, py) connectSouthOptions >>= \newPos ->
      case HM.lookup newPos grid of
        Just '|' -> (newPos :) <$> traverseG (Grid newPos grid) South
        Just 'L' -> (newPos :) <$> traverseG (Grid newPos grid) East
        Just 'J' -> (newPos :) <$> traverseG (Grid newPos grid) West
        _ -> Just [newPos]
  East ->
    connect (px, py + 1) connectEastOptions >>= \newPos ->
      case HM.lookup newPos grid of
        Just '-' -> (newPos :) <$> traverseG (Grid newPos grid) East
        Just 'J' -> (newPos :) <$> traverseG (Grid newPos grid) North
        Just '7' -> (newPos :) <$> traverseG (Grid newPos grid) South
        _ -> Just [newPos]
  West ->
    connect (px, py - 1) connectWestOptions >>= \newPos ->
      case HM.lookup newPos grid of
        Just '-' -> (newPos :) <$> traverseG (Grid newPos grid) West
        Just 'F' -> (newPos :) <$> traverseG (Grid newPos grid) South
        Just 'L' -> (newPos :) <$> traverseG (Grid newPos grid) North
        _ -> Just [newPos]
  where
    connect :: Position -> String -> Maybe Position
    connect nPos dirs = HM.lookup nPos grid >>= (\rc -> if rc `elem` dirs then Just nPos else Nothing)
    connectNorthOptions = "|F7S"
    connectSouthOptions = "|JLS"
    connectEastOptions = "-7JS"
    connectWestOptions = "-FLS"

-- Solve 1
-- As the path is symmetrical, the farthest is the length / 2.
solve1 :: String -> String
solve1 raw =
  show $
    fmap ((`div` 2) . length) $
      head $
        dropWhile isNothing $
          map
            (traverseG g)
            [North, West, South, East]
  where
    g = parseInput raw

-- Solve 2

-- The number of enclosed tiles is just the area of the shape.
-- https://en.wikipedia.org/wiki/Shoelace_formula
-- The Shoelace formual is able to compute the are from the coordinates.

shoelaceArea :: [Position] -> Int
shoelaceArea xys = abs (go xys 0 + det (last xys) (head xys)) `div` 2
  where
    det (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
    go :: [Position] -> Int -> Int
    -- go p@(p1 : p2 : ps) acc = trace (show p) $ go (p2 : ps) (acc + det p1 p2)
    go (p1 : p2 : ps) acc = go (p2 : ps) (acc + det p1 p2)
    go [_] acc = acc
    -- go [a] acc = trace ("finished with: " ++ show acc ++ " and " ++ show a) acc
    go [] acc = trace ("finished with: " ++ show acc) acc

solve2 :: String -> String
solve2 raw =
  show $ 1 + shoelaceArea path - (length path `div` 2)
  where
    Just path =
      head $
        dropWhile isNothing $
          map
            (traverseG g)
            [North, West, South, East]
    g = parseInput raw

-- Test data

input1_ =
  [ "-L|F7",
    "7S-7|",
    "L|7||",
    "-L-J|",
    "L|-JF"
  ]

raw1_ = unlines input1_

input2_ =
  [ "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  ]

raw2_ = unlines input2_

input3_ =
  [ "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  ]

input4_ =
  [ ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ..."
  ]
