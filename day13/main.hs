{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.List (sortBy)

data Sig = SL [Sig] | SI Int
  deriving (Show, Eq)

data SigCmp = Good | Bad | Neutral
  deriving (Show, Eq)

listParser :: Parser Sig
listParser = do
  char '['
  inside <- sepBy parser (char ',')
  char ']'
  return $! SL inside

numParser :: Parser Sig
numParser = do
  val <- decimal
  return $! SI val

parser :: Parser Sig
parser = choice [listParser, numParser]

parseSignal :: T.Text -> Sig
parseSignal s = case (parse parser s) of
  Done _ r -> r
  _ -> error "Bad input"

parsePairs :: [T.Text] -> (Sig, Sig)
parsePairs (x:y:[]) = (parseSignal x, parseSignal y)
parsePairs _ = error "Bad input"

makeList :: Sig -> Sig
makeList (SI a) = SL [SI a]
makeList a = a

compareSignals :: Sig -> Sig -> SigCmp
compareSignals (SI a) (SI b)
  | a < b = Good
  | a > b = Bad
  | otherwise = Neutral
compareSignals (SL a) (SL b) = let
  res = foldl c Neutral (zip a b)
  lenA = length a
  lenB = length b
  in case res of
    Neutral | lenA < lenB -> Good
    Neutral | lenA > lenB -> Bad
    _ -> res
  where
    c Neutral (a, b) = compareSignals a b
    c acc _ = acc
compareSignals a b = compareSignals (makeList a) (makeList b)

orderSignals :: Sig -> Sig -> Ordering
orderSignals a b = case (compareSignals a b) of
  Good -> LT
  Neutral -> EQ
  Bad -> GT

main = do
  src <- TIO.readFile "assets/day13"
  let signals = map T.strip $ T.splitOn "\n\n" src
  let signalsLines = map T.lines signals
  let signalPairs = map parsePairs signalsLines

  let signalStatuses = map (uncurry compareSignals) signalPairs
  let signalStatuses' = zip [1..] signalStatuses
  let goodSignals = filter (\(_, s) -> s == Good) signalStatuses'
  let goodIndicies = map (\(i, _) -> i) goodSignals
  let part1 = foldl1 (+) goodIndicies
  putStrLn $ "Part 1: " ++ (show part1)
  
  let first = SL [SL [SI 2]]
  let second = SL [SL [SI 6]]
  let allSignals = (concatMap (\(x, y) -> [x, y]) signalPairs) ++ [first, second]
  let sortedSignals = sortBy orderSignals allSignals
  let sortedWithIdx = zip [1..] sortedSignals
  let markers = filter (\(_, e) -> e == first || e == second) sortedWithIdx
  let markerIndicies = map (\(i, _) -> i) markers
  let part2 = foldl1 (*) markerIndicies
  putStrLn $ "Part 2: " ++ (show part2)
