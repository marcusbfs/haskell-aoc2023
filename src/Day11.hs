module Day11 (solve1, solve2) where

import Data.List (genericLength, transpose)

type Position = (Integer, Integer)

data Node = Node
  { pos_ :: Position, -- Position in the array
    label_ :: Char, -- label. But not really needed.
    uniqueId_ :: Integer, -- unique Id useful for debug
    numOfSpaceRows_ :: Integer, -- number of empty rows that comes before this node
    numOfSpaceCols_ :: Integer -- number of empty cols that comes before this node
  }
  deriving (Show, Eq)

-- Parses the input into a more managable data.
parseInput :: [String] -> [Node]
parseInput raw = nodes_
  where
    (_, nodes_, _, _) = go (0, [], raw, 1)

    parseLine :: (Integer, Integer, [Node], String, Integer) -> (Integer, Integer, [Node], String, Integer)
    parseLine g@(_, _, _, "", _) = g
    parseLine (row, col, nodes, c : cs, id_) =
      parseLine
        ( row,
          col + 1,
          if c /= '.'
            then
              Node
                (row, col)
                c
                id_
                (genericLength $ filter (< row) emptyRows)
                (genericLength $ filter (< col) emptyCols)
                : nodes
            else nodes,
          cs,
          if c /= '.' then id_ + 1 else id_
        )

    go :: (Integer, [Node], [String], Integer) -> (Integer, [Node], [String], Integer)
    go g@(_, _, [], _) = g
    go (row, nodes, l : lss, id_) = go (row + 1, g, lss, id_')
      where
        (_, _, g, _, id_') = parseLine (row, 0, nodes, l, id_)

    (emptyRows, emptyCols) = emptyIndexes

    -- Store the indexes of empty rows and empty cols
    emptyIndexes :: ([Integer], [Integer])
    emptyIndexes = (f raw, f $ transpose raw)
      where
        f = map fst . filter snd . zipWith (\i n -> (i, all (== '.') n)) [0 ..]

-- Expand the spaces: any rows or columns that contain no galaxies should all
-- actually be Nx as big. It only updates the coordinates of the nodes.
expandIndexes :: Integer -> [Node] -> [Node]
expandIndexes n = go
  where
    go [] = []
    go (Node (x, y) c id_ nsr nsc : ns) =
      Node
        ( if nsr > 0 then x + nsr * n - nsr else x,
          if nsc > 0 then y + nsc * n - nsc else y
        )
        c
        id_
        nsr
        nsc
        : go ns

-- Compute the length of the shortest path between two points.
lengthOfShortestPath :: Position -> Position -> Integer
lengthOfShortestPath (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Generic solve
solve :: Integer -> String -> String
solve factor raw = show $ go nodes 0
  where
    nodes = expandIndexes factor $ parseInput $ lines raw

    go [] acc = acc
    go (Node p _ _ _ _ : ns) acc =
      go ns $
        foldl
          ( \a (Node pi_ _ _ _ _) ->
              a
                + lengthOfShortestPath pi_ p
          )
          acc
          ns

-- Solve 1
solve1 :: String -> String
solve1 = solve 2

-- Solve 2
solve2 :: String -> String
solve2 = solve 1000000

-- Test data

input1_ =
  [ "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  ]

raw1_ = unlines input1_
