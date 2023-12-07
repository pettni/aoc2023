module Day7 (solve1, solve2) where

import Data.Either (fromRight)
import Data.Function (on)
import Data.List (find, nub, sortBy, sortOn)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Tuple (swap)
import qualified Text.Parsec as Parsec

-- Types

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA deriving (Eq, Show, Ord, Enum)

data HandType = PHHighCard | PHPair | PHTwoPair | PHThreeOfAKind | PHFullHouse | PHFourOfAKind | PHFiveOfAKind deriving (Eq, Show, Ord, Enum)

data Game = Game {hand :: [Card], bid :: Int} deriving (Show)

-- Parsing

parseInput :: Text -> [Game]
parseInput ss = fromRight (error "Failed to parse") $ Parsec.parse handsData "Failure" $ T.unpack ss
  where
    handsData = Parsec.endBy handLine Parsec.endOfLine
    handLine = do
      cards <- Parsec.many (Parsec.noneOf " ")
      Parsec.spaces
      bid <- Parsec.many (Parsec.noneOf "\n")
      let hand = map parseCard cards
      return $ Game hand (read bid :: Int)

parseCard :: Char -> Card
parseCard c = case c of
  '2' -> C2
  '3' -> C3
  '4' -> C4
  '5' -> C5
  '6' -> C6
  '7' -> C7
  '8' -> C8
  '9' -> C9
  'T' -> CT
  'J' -> CJ
  'Q' -> CQ
  'K' -> CK
  'A' -> CA
  _ -> error $ "Invalid card: " ++ show c

-- Part 1

solve1 :: Text -> Int
solve1 = foldl fFold 0 . zip [1 ..] . sortOn (fVal . hand) . parseInput
  where
    fFold val (i, game) = val + i * bid game
    fVal cards = (determineHand . map snd . countUnique $ cards, cards)

countUnique :: (Eq a) => [a] -> [(a, Int)]
countUnique xs = [(x, length $ filter (== x) xs) | x <- nub xs]

determineHand :: [Int] -> HandType
determineHand cardCounts
  | maxSameCard == 5 = PHFiveOfAKind
  | maxSameCard == 4 = PHFourOfAKind
  | maxSameCard == 3 && length cardCounts == 2 = PHFullHouse
  | maxSameCard == 3 = PHThreeOfAKind
  | maxSameCard == 2 && length cardCounts == 3 = PHTwoPair
  | maxSameCard == 2 = PHPair
  | otherwise = PHHighCard
  where
    maxSameCard = maximum cardCounts

-- Part 2

-- deck with modified ordering
newtype JokerDeckCard = JokerDeckCard Card deriving (Eq)

instance Ord JokerDeckCard where
  compare (JokerDeckCard c1) (JokerDeckCard c2)
    | c1 == CJ && c2 /= CJ = LT
    | c2 == CJ && c1 /= CJ = GT
    | otherwise = compare c1 c2

solve2 :: Text -> Int
solve2 = foldl fFold 0 . zip [1 ..] . sortOn (fVal . hand) . parseInput
  where
    fFold val (i, game) = val + i * bid game
    fVal cards = (determineHand . map snd . wildCardCounts . countUnique $ cards, map JokerDeckCard cards)

wildCardCounts :: [(Card, Int)] -> [(Card, Int)]
wildCardCounts cardCounts
  | numJacks == 5 = [(CA, 5)]
  | otherwise = (card0, count0 + numJacks) : drop 1 orderedCountsNoJacks
  where
    numJacks = maybe 0 snd $ find ((== CJ) . fst) cardCounts
    orderedCountsNoJacks = sortBy (flip compare `on` swap) . filter ((/= CJ) . fst) $ cardCounts
    (card0, count0) = head orderedCountsNoJacks
