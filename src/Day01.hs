module Day01 (solve1, solve2) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)

solve1 :: String -> String
solve1 = show . sum . map f . lines
  where
    f :: String -> Int
    f s = read [n1, n2]
      where
        n1 = head $ dropWhile (not . isDigit) s
        n2 = head $ dropWhile (not . isDigit) $ reverse s

------------------------------------

type ENum = (Int, String)

type Nums = [ENum]

strsNums :: Nums
strsNums = zip [1 ..] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

revStrsNums :: Nums
revStrsNums = map (\(i, s) -> (i, reverse s)) strsNums

matchSingle :: String -> ENum -> Maybe Int
matchSingle inp (i, s) = if s `isPrefixOf` inp then Just i else Nothing

matchSingleNum :: String -> Maybe Int
matchSingleNum [] = Nothing
matchSingleNum (x : _) = if isDigit x then Just (read [x]) else Nothing

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) Nothing Nothing = Nothing
(<|>) x Nothing = x
(<|>) Nothing y = y
(<|>) x _ = x

matchStr :: Nums -> String -> Maybe Int
matchStr _ [] = Nothing
matchStr strs input@(_ : xs) =
  matchSingleNum input
    <|> if null nonEmptyList
      then matchStr strs xs
      else head nonEmptyList
  where
    nonEmptyList = dropWhile isNothing $ map (matchSingle input) strs

solve2 :: String -> String
solve2 = show . foldl (\acc i -> (+) <$> acc <*> i) (pure 0) . map f . lines
  where
    f :: String -> Maybe Int
    f s = do
      n1 <- matchStr strsNums s
      n2 <- matchStr revStrsNums $ reverse s
      return (n1 * 10 + n2)
