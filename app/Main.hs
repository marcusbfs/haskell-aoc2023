{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Day08 qualified
import Day09 qualified
-- import Day10 qualified
import System.Console.CmdArgs (Data, Typeable, cmdArgs)
import Text.Printf

data AOCRunner = AOCRunner {day_number :: Int} deriving (Show, Data, Typeable)

type Solver = String -> String

solvers :: [(Solver, Solver)]
solvers =
  [ (Day01.solve1, Day01.solve2),
    (Day02.solve1, Day02.solve2),
    (Day03.solve1, Day03.solve2),
    (Day04.solve1, Day04.solve2),
    (Day05.solve1, Day05.solve2),
    (Day06.solve1, Day06.solve2),
    (Day07.solve1, Day07.solve2),
    (Day08.solve1, Day08.solve2),
    (Day09.solve1, Day09.solve2)
    -- (Day10.solve1, Day10.solve2)
  ]

getSolver :: Int -> (Solver, Solver)
getSolver n
  | n <= 0 || n > length solvers = error (printf "invalid day: %02d. Days available: 01 to %02d" n (length solvers))
  | otherwise = solvers !! (n - 1)

mainRunSingle :: Int -> IO ()
mainRunSingle n =
  let (sol1, sol2) = getSolver n
   in do
        c <- readFile $ printf "input/%02d.txt" n
        putStrLn $ printf "\nDay %02d" n
        putStrLn $ "1: " ++ sol1 c
        putStrLn $ "2: " ++ sol2 c

mainRunAll :: IO ()
mainRunAll = sequence_ [mainRunSingle i | i <- [1 .. (length solvers)]]

main :: IO ()
main = do
  args_ <- cmdArgs AOCRunner {day_number = 0}
  case day_number args_ of
    0 -> mainRunAll
    x -> mainRunSingle x
