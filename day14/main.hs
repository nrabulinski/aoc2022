{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashSet as HashSet
import Data.Text.Read (decimal)
import Data.Maybe (isJust, catMaybes)
import Control.Monad (foldM)

forceNumber :: T.Text -> Int
forceNumber n = case (decimal n) of
  Right (v, _) -> v
  _ -> error "Bad input"
  
pair :: a -> b -> (a, b)
pair a b = (a, b)

forcePair :: [a] -> (a, a)
forcePair (x:y:[]) = (x, y)
forcePair _ = error "Bad input"

parsePair :: T.Text -> (Int, Int)
parsePair src = forcePair . map forceNumber $ T.splitOn "," src

r :: Int -> Int -> [Int]
r a b
  | a > b = [b..a]
  | otherwise = [a..b]
  
rockLine :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
rockLine ((fromX, fromY), (toX, toY)) = pair <$> r fromX toX <*> r fromY toY

rockFormation :: [(Int, Int)] -> [(Int, Int)]
rockFormation s@(_:xs) =
  concatMap rockLine $ zip s xs

type Point = (Int, Int)
type Points = HashSet.HashSet Point
type EitherPoints = Either Points Points

part1Step :: Int -> Points -> Int -> EitherPoints
part1Step maxY = sandStepImpl
  where
    sandStepImpl points _ = step (500, 0)
      where
        step p@(x, y) =
          if y >= maxY then
            Left points
          else if not (HashSet.member (x, y + 1) points) then
            step (x, y + 1)
          else if not (HashSet.member (x - 1, y + 1) points) then
            step (x - 1, y + 1)
          else if not (HashSet.member (x + 1, y + 1) points) then
            step (x + 1, y + 1)
          else Right (HashSet.insert p points)

part2Step :: Int -> Points -> Int -> EitherPoints
part2Step maxY = sandStepImpl
  where
    sandStepImpl points _ = step (500, 0)
      where
        step p@(x, y) =
          if y == maxY then
            Right (HashSet.insert p points)
          else if not (HashSet.member (x, y + 1) points) then
            step (x, y + 1)
          else if not (HashSet.member (x - 1, y + 1) points) then
            step (x - 1, y + 1)
          else if not (HashSet.member (x + 1, y + 1) points) then
            step (x + 1, y + 1)
          else if p == (500, 0) then
            Left (HashSet.insert p points)
          else Right (HashSet.insert p points)

main = do
  src <- TIO.readFile "assets/day14"
  let rockInput = map (map parsePair . T.splitOn " -> ") $ T.splitOn "\n" $ T.strip src

  let xs = concatMap (map fst) rockInput
  let (startX, endX) = (minimum xs, maximum xs)
  let width = endX - startX

  let ys = concatMap (map snd) rockInput
  let (startY, endY) = (0, maximum ys)
  let height = endY - startY
  
  let rocks = HashSet.fromList $ concatMap rockFormation rockInput
  
  let part1Step' = part1Step endY
  let sandPile1 = either id id $ foldM part1Step' rocks [0..]
  let sandGrains1 = (length sandPile1) - (length rocks)

  -- mapM_ TIO.putStrLn $ 
  --   map 
  --     (\y ->
  --       T.concat $
  --       map (\x -> 
  --         if (HashSet.member (x, y) sandPile1) then
  --           if (HashSet.member (x, y) rocks) then "█" else "o"
  --         else " ") 
  --       [startX..endX])
  --     [startY..endY]

  putStrLn $ "Part 1: " ++ (show sandGrains1)

  let part2Step' = part2Step (endY + 1)
  let sandPile2 = either id id $ foldM part2Step' rocks [0..]
  let sandGrains2 = (length sandPile2) - (length rocks)

  -- mapM_ TIO.putStrLn $ 
  --   (map 
  --     (\y ->
  --       T.concat $
  --       map (\x -> 
  --         if (HashSet.member (x, y) sandPile2) then
  --           if (HashSet.member (x, y) rocks) then "#" else "o"
  --         else ".") 
  --       [(startX - 5)..(endX + 5)])
  --     [startY..(endY + 1)])
  --   ++ ["###################################################"]

  putStrLn $ "Part 2: " ++ (show sandGrains2)
