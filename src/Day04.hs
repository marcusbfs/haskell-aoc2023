module Day04 (solve1, solve2) where

import Utils (splitStrAtSep, trimWhitespace)

data Card = Card {id_ :: Int, winNums_ :: [Int], myNums_ :: [Int], numReps_ :: Int} deriving (Show)

-- Parse the card string
parseCard :: String -> Card
parseCard s =
  Card
    (read id_)
    (f $ head vals)
    (f $ vals !! 1)
    0
  where
    id_ = trimWhitespace $ takeWhile (/= ':') $ drop 5 s
    vals = splitStrAtSep (tail $ dropWhile (/= ':') s) '|'
    f :: String -> [Int]
    f = map read . filter (not . null) . flip splitStrAtSep ' ' . trimWhitespace

-- Compute the total worth value of card as explained in Part 1
computeWorthPoints :: Card -> Int
computeWorthPoints (Card _ winNums myNums _) = if null mwn then 0 else 2 ^ (length mwn - 1)
  where
    mwn = filter (`elem` myNums) winNums

-- Part 1
solve1 :: String -> String
solve1 = show . sum . map (computeWorthPoints . parseCard) . lines

-- Part 2

-- Expand the replicated cards and returns the total
-- TODO: this looks like a fold function. Maybe it can be used here.
expandCards :: [Card] -> Int -> Int
expandCards [] pCards = pCards
expandCards (Card _ winNums myNums numReps : cs) pCards = expandCards (a ++ b) (1 + numReps + pCards)
  where
    a = map (\c -> c {numReps_ = numReps_ c + 1 + numReps}) $ take numMatching cs
    b = drop numMatching cs
    numMatching = length $ filter (`elem` myNums) winNums

solve2 :: String -> String
solve2 = show . flip expandCards 0 . map parseCard . lines

-- Test data
input_ =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

raw_ = unlines input_
