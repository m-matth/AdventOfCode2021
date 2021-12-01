module Day1
  ( day1
  ) where

parseInput :: String -> [Int]
parseInput s = map (\x -> read x :: Int) (lines s)

countIncrease' :: (Num a, Ord a) => (a -> (a, a) -> a)
countIncrease' acc (curr, next) =
  if next > curr
    then acc + 1
    else acc

countIncrease :: (Num a, Ord a) => [a] -> a
countIncrease i = foldl countIncrease' 0 (zip i (tail i))

countIncrease2 :: (Num a, Ord a) => [a] -> a
countIncrease2 i = foldl countIncrease' 0 (zip lst (tail lst))
  where
    lst = map (\(x, y, z) -> x + y + z) $ zip3 i (tail i) (tail (tail i))

day1 :: IO ()
day1 = do
  value <- parseInput <$> readFile "input_day1.txt"
  let result1 = countIncrease value
  print $ "day1 - part1 : " ++ show result1
  let result2 = countIncrease2 value
  print $ "day1 - part2 : " ++ show result2
