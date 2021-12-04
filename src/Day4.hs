{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Day4
  ( day4
  ) where

import Data.Bifunctor
import Data.List
import Data.List.Split

type Value = Integer

type Marked = Bool

type Score = Integer

type Line = [(Value, Marked)]

type Board = [[(Value, Marked)]]

data Bingo =
  Bingo
    { draw :: [Value]
    , boards :: [Board]
    }
  deriving (Show)

parseData :: [String] -> Bingo
parseData dat =
  Bingo
    { draw = (map read $ splitOn "," (head dat)) :: [Value]
    , boards = board' (tail dat)
    }
  where
    board' =
      map (map (map (\x -> (read x :: Integer, False)) . words)) .
      filter ([] /=) . splitOneOf [""]

playGame :: Bingo -> [Score]
playGame Bingo {..} = pickNumber draw boards
  where
    pickNumber :: [Value] -> [Board] -> [Score]
    pickNumber [] _ = []
    pickNumber (d:ds) b = do
      let bs = map (map (markLine d)) b
          (win, left) = partition ((==) True . snd) $ zip bs (hasAligned bs)
      map (computeScoreWith d . fst) win ++ pickNumber ds (map fst left)
    computeScoreWith :: Value -> Board -> Score
    computeScoreWith v =
      (*) v .
      foldl
        (foldr
           (\v' ->
              (+)
                (if not (snd v')
                   then fst v'
                   else 0)))
        0
    hasAligned :: [Board] -> [Bool]
    hasAligned = map (\b -> hasAlignedH b || hasAlignedV b || hasAlignedDiag b)
    lineAligned :: Line -> Bool
    lineAligned = all ((True ==) . snd)
    hasAlignedH :: Board -> Bool
    hasAlignedH = any ((True ==) . lineAligned)
    hasAlignedV :: Board -> Bool
    hasAlignedV = any ((True ==) . lineAligned) . transpose
    hasAlignedDiag :: Board -> Bool
    hasAlignedDiag b = lineAligned $ genDiag1 [] 0 b
                       -- || (lineAligned $ genDiag2 [] 0 b) !!?
      where
        genDiag1 :: Line -> Int -> Board -> Line
        genDiag1 acc _ [] = acc
        genDiag1 acc y (x:xs) = genDiag1 (x !! y : acc) (y + 1) xs
        genDiag2 :: Line -> Int -> Board -> Line
        genDiag2 acc _ [] = acc
        genDiag2 acc y (x:xs) = genDiag2 (reverse x !! y : acc) (y + 1) xs
    markLine :: Value -> Line -> Line
    markLine val =
      map
        (\t ->
           if val == fst t
             then second (const True) t
             else t)

day4 :: IO ()
day4 = do
  day3Data <- lines <$> readFile "input_day4.txt"
  let gameScores = playGame (parseData day3Data)
  print $ "day4 - part1 : " ++ show (head gameScores)
  print $ "day4 - part2 : " ++ show (last gameScores)
