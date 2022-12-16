{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashSet as HashSet
import Data.Text.Read (signed, decimal)
import Data.Maybe
import Control.Applicative

type Point = (Int, Int)

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

pair :: a -> b -> (a, b)
pair a b = (a, b)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

forceNumber :: T.Text -> Int
forceNumber n = case (signed decimal n) of
  Right (v, _) -> v
  e -> error $ "Bad input " ++ (show n) ++ ": " ++ (show e)
  
forcePair :: [a] -> (a, a)
forcePair (x:y:[]) = (x, y)
forcePair _ = error "Bad input"

dropLast s = maybe s id $ T.stripSuffix "," s <|> T.stripSuffix ":" s

parseCoord :: T.Text -> Maybe Int
parseCoord s = fmap (forceNumber . dropLast) $ T.stripPrefix "x=" s <|> T.stripPrefix "y=" s

parseLine :: T.Text -> (Point, Point)
parseLine s = let
  (x1:y1:x2:y2:[]) = mapMaybe parseCoord $ T.splitOn " " s
  in ((x1, y1), (x2, y2))
  
p1y = 2000000

minCoord = 0
maxCoord = 4000000

main = do
  src <- TIO.readFile "assets/day15"
  let coords = map parseLine $ T.splitOn "\n" $ T.strip src
  let allPoints = concatMap (\(a, b) -> [a, b]) coords
  
  let dists = map (\(a@(_, y), b) -> (a, (dist a b) - (abs $ y - p1y))) coords
  let relevant = filter (\((_, y), d) -> d >= 0) dists
  let taken = HashSet.fromList $ concatMap (\((x, _), d) -> [(x - d)..(x + d)]) relevant
  let occupied = HashSet.fromList $ map fst $ filter (\(_, y) -> y == p1y) allPoints
  
  let part1 = HashSet.difference taken occupied
  
  print (length part1)
