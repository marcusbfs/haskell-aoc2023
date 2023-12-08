module Day02 (solve1, solve2) where


import Utils (splitStrAtSep, trimWhitespace)

-- Parsers

data Color = Red | Green | Blue deriving (Show, Eq)

type Set = [(Int, Color)]

data Game = Game {id :: Int, sets :: [Set]} deriving (Show)


str2color :: String -> Color
str2color "red" = Red
str2color "blue" = Blue
str2color "green" = Green
str2color _ = error "unknown color"


-- Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
parseSingleGame :: String -> Game
parseSingleGame s = Game id_ (map parseSingleSet s')
  where
    id_ = read $ takeWhile (/= ':') $ drop 5 s :: Int
    s' = splitStrAtSep (drop 2 $ dropWhile (/= ':') s) ';'
    -- 1 blue, 2 green
    parseSingleSet :: String -> Set
    parseSingleSet s_ =
      map
        ( \s_'' ->
            let s_''' = map trimWhitespace $ splitStrAtSep s_'' ' '
             in (read $ head s_''', str2color $ s_''' !! 1)
        )
        s_'
      where
        s_' = map trimWhitespace $ splitStrAtSep s_ ','

parseGames :: [String] -> [Game]
parseGames = map parseSingleGame

validSet :: Set -> Bool
validSet s = length s == length (takeWhile validColor s)
  where
    validColor :: (Int, Color) -> Bool
    validColor (x, Red) = x <= 12
    validColor (x, Green) = x <= 13
    validColor (x, Blue) = x <= 14

isGameValid :: Game -> Bool
isGameValid (Game _ sts) = all validSet sts

solve1 :: String -> String
solve1 = show . foldl (\acc (Game id_ _) -> acc + id_) 0 . filter isGameValid . parseGames . lines

---------------------

powerOfMinSetOfCubes :: [Set] -> Int
powerOfMinSetOfCubes sts = maxSetColor Red sts * maxSetColor Blue sts * maxSetColor Green sts
  where
    maxSetColor :: Color -> [Set] -> Int
    maxSetColor color = maximum . concatMap (map (\(i, c) -> if c == color then i else 0))

solve2 :: String -> String
solve2 = show . foldl (\acc (Game _ sts) -> acc + powerOfMinSetOfCubes sts) 0 . parseGames . lines

input_ =
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

raw_ = unlines input_
