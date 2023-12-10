module Day07 where

import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (fromMaybe)
import Utils (removeCommon, splitStrAtSep)

data Play = Play {hand_ :: String, bid_ :: Int, type_ :: HandType} deriving (Show)

data HandType
  = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  deriving (Show, Eq, Enum, Bounded)

handTypesDescOrder :: [HandType]
handTypesDescOrder = [(minBound :: HandType) ..]

-- The last '1' represents 'J' in part 2.
kindDescOrder :: String
kindDescOrder = "AKQJT987654321"

-- Allow cards to be compared and sorted.
instance Eq Play where
  (Play h1 _ t1) == (Play h2 _ t2) = (sort h1 == sort h2) && t1 == t2

instance Ord Play where
  (Play h1 _ t1) <= (Play h2 _ t2) =
    if t1 /= t2
      then
        let oh1 = elemIndex t1 handTypesDescOrder
            oh2 = elemIndex t2 handTypesDescOrder
         in fromMaybe undefined ((>) <$> oh1 <*> oh2)
      else -- Same type

        let (ch1, ch2) = removeCommon h1 h2
            oh1 = elemIndex (head ch1) kindDescOrder
            oh2 = elemIndex (head ch2) kindDescOrder
         in fromMaybe undefined ((>) <$> oh1 <*> oh2)

-- Parse a single card.

getCardType :: String -> HandType
getCardType hand
  | length g == 1 = FiveOfAKind
  | length g == length hand = HighCard
  | length g == length hand - 1 = OnePair
  | length g == 2 = if (gcount !! 1) == 4 then FourOfAKind else FullHouse
  | length g == 3 = if (gcount !! 2) == 3 then ThreeOfAKind else TwoPair
  where
    g = group $ sort hand
    gcount = sort $ map length g

parseSingleCard :: String -> Play
parseSingleCard input = Play hand (read bid) (getCardType hand)
  where
    (hand : bid : _) = splitStrAtSep input ' '

parseInput :: String -> [Play]
parseInput = map parseSingleCard . lines

-- Solve 1
solve1 :: String -> String
solve1 = show . sum . zipWith (\i (Play _ bid _) -> i * bid) [1 ..] . sort . parseInput

-- Solve 2

-- Given a hand, replace the J's to get the best possible hand.
replaceJ :: String -> String
replaceJ hand = map replJ hand
  where
    -- g = map head $ sortBy (compare `on` length) $ group $ sort hand
    g =
      map head
        $ sortBy
          ( \a b ->
              let la = length a
                  lb = length b
               in if la /= lb
                    then compare la lb
                    else
                      let (ch1, ch2) = removeCommon a b
                          oh1 = elemIndex (head ch1) kindDescOrder
                          oh2 = elemIndex (head ch2) kindDescOrder
                       in fromMaybe undefined (compare <$> oh2 <*> oh1)
          )
        $ group
        $ sort hand
    charMostFrequent
      | last g /= 'J' = last g
      | length g > 1 = g !! (length g - 2)
      | otherwise = last g
    replJ 'J' = charMostFrequent
    replJ a = a

-- Here, replace each play with the better hand type and replace the J's to 1's
parseSingleCard2 :: String -> Play
parseSingleCard2 input =
  origPlay
    { hand_ = map replJto1 origHand,
      type_ = getCardType (replaceJ origHand)
    }
  where
    origPlay = parseSingleCard input
    origHand = hand_ origPlay
    replJto1 'J' = '1'
    replJto1 a = a

solve2 :: String -> String
solve2 = show . sum . zipWith (\i (Play _ bid _) -> i * bid) [1 ..] . sort . map parseSingleCard2 . lines

-- Test data

input_ =
  [ "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  ]

raw_ = unlines input_
