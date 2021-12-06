{-# LANGUAGE LambdaCase #-}

module Day6
  ( day6
  ) where

import Data.List.Split
import qualified Data.Map as M

parse :: String -> [Age]
parse = map read . splitOn "," . head . lines

type Age = Integer

type Count = Integer

groupByValues :: [Age] -> M.Map Age Count
groupByValues xs = M.fromListWith (+) [(x, 1) | x <- xs]

afterOneDay :: M.Map Age Count -> M.Map Age Count
afterOneDay xs =
  M.fromListWith (+) $
  concatMap
    ((\case
        [(0, c)] -> [(8, c), (6, c)]
        [(x', c)] -> [(x' - 1, c)]
        x' -> x') .
     (: []))
    (M.toList xs)

day6 :: IO ()
day6 = do
  day6Data <- parse <$> readFile "input_day6.txt"
  let byAge = groupByValues day6Data
  print $ "day6 - part1 : " ++ show (afterDay 80 byAge)
  print $ "day6 - part2 : " ++ show (afterDay 256 byAge)
  where
    afterDay x byAge =
      sum $ M.elems $ foldl (const . afterOneDay) byAge ([1 .. x] :: [Integer])
