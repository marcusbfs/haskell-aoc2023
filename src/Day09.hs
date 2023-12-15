module Day09 (solve1, solve2) where

import Utils (getNumsFromString)

parseInput :: String -> [[Int]]
parseInput = map (`getNumsFromString` ' ') . lines

solve :: ([[Int]] -> Int) -> [Int] -> Int
solve fun = fun . takeWhile (not . all (== 0)) . iterate f
  where
    f x_ = zipWith (-) (tail x_) x_

-- Part 1

-- Examining the problem, we can see that once we have the final line
-- (containing only 0's), we can just sum all elements at the end of the arrays.
solve1 :: String -> String
solve1 = show . sum . map (solve (sum . map last)) . parseInput

-- Part 2

solve2 :: String -> String
solve2 =
  show
    . sum
    . map
      ( solve (foldl1 (\acc x -> x - acc) . map head . reverse)
      )
    . parseInput

-- Test data

input_ =
  [ "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
  ]
