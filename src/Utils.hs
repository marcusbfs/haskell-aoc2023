module Utils where

import Data.Char (isSpace)

-- splitStrAtSep "1,2,3" ',' -> ["1", "2", "3"]
splitStrAtSep :: String -> Char -> [String]
splitStrAtSep str sep = go str []
  where
    go input acc = if s /= "" then go (tail s) (acc ++ [f]) else acc ++ [f]
      where
        (f, s) = span (/= sep) input

-- trimWhitespace " foobar " -> "foobar"
trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- getNumsFromString "1 2 3" ' ' -> [1, 2, 3]
getNumsFromString :: (Read a, Num a) => String -> Char -> [a]
getNumsFromString input sep = map read strs
  where
    strs = filter (not . null) $ splitStrAtSep input sep

-- removeCommon "abcfoo" "abcbar" -> ("foo", "bar")
removeCommon :: (Eq a) => [a] -> [a] -> ([a], [a])
removeCommon [] b = ([], b)
removeCommon a [] = (a, [])
removeCommon at@(a : as) bt@(b : bs) = if a == b then removeCommon as bs else (at, bt)

-- takeWhileInclusive isDigt "123end" -> "123e"
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
