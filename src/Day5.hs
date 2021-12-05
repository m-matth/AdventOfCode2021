{-# LANGUAGE TupleSections #-}

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

genPoints :: Coord -> [Point]
genPoints ((x, y), (x', y'))
  | x == x' && y > y' = map (x, ) [y' .. y]
  | x == x' && y' > y = map (x, ) [y .. y']
  | y == y' && x > x' = map (, y) [x' .. x]
  | y == y' && x' > x = map (, y) [x .. x']
  | x >= x' && y >= y' = [(x - z, y - z) | z <- [0 .. (x - x')]]
  | x >= x' && y' >= y = [(x - z, y + z) | z <- [0 .. (x - x')]]
  | x' >= x && y' >= y = [(x + z, y + z) | z <- [0 .. (x' - x)]]
  | x' >= x && y >= y' = [(x + z, y - z) | z <- [0 .. (x' - x)]]
  | otherwise = []

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

day5 :: IO ()
day5 = do
  day5Data <- map parse . lines <$> readFile "input_day5.txt"
  let coords = filter filterCoord day5Data
  print $
    "day5 - part1 : " ++
    show (length $ overlap $ frequency $ concatMap genPoints coords)
  print $
    "day5 - part2 : " ++
    show (length $ overlap $ frequency $ concatMap genPoints day5Data)
  where
    overlap = filter ((> 1) . snd)
