module Day5
  ( day5
  ) where

import Data.List.Split
import qualified Data.Map as M

type Point = (Integer, Integer)

type Coord = (Point, Point)

parse :: String -> Coord
parse ln =
  let [x, _, x'] = words ln
      [a, b] = map read $ splitOn "," x
      [c, d] = map read $ splitOn "," x'
   in ((a, b), (c, d))

filterCoord :: Coord -> Bool
filterCoord ((x, y), (x', y')) = x == x' || y == y'

type Points = M.Map Point Count

type Count = Integer

genPoints :: Points -> Coord -> Points
genPoints m ((x, y), (x', y'))
  | x == x' && y > y' = foldr (\v -> M.insertWith (+) (x, v) 1) m [y' .. y]
  | x == x' && y' > y = foldr (\v -> M.insertWith (+) (x, v) 1) m [y .. y']
  | y == y' && x > x' = foldr (\v -> M.insertWith (+) (v, y) 1) m [x' .. x]
  | y == y' && x' > x = foldr (\v -> M.insertWith (+) (v, y) 1) m [x .. x']
  | x >= x' && y >= y' =
    foldr (flip (M.insertWith (+)) 1) m [(x - z, y - z) | z <- [0 .. (x - x')]]
  | x >= x' && y' >= y =
    foldr (flip (M.insertWith (+)) 1) m [(x - z, y + z) | z <- [0 .. (x - x')]]
  | x' >= x && y' >= y =
    foldr (flip (M.insertWith (+)) 1) m [(x + z, y + z) | z <- [0 .. (x' - x)]]
  | x' >= x && y >= y' =
    foldr (flip (M.insertWith (+)) 1) m [(x + z, y - z) | z <- [0 .. (x' - x)]]
  | otherwise = m

overlap :: Points -> Int
overlap = M.size . M.filter (> 1)

day5 :: IO ()
day5 = do
  day5Data <- map parse . lines <$> readFile "input_day5.txt"
  let coords = filter filterCoord day5Data
  print $ "day5 - part1 : " ++ show (overlap $ foldl genPoints M.empty coords)
  print $ "day5 - part2 : " ++ show (overlap $ foldl genPoints M.empty day5Data)
