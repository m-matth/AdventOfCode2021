module Day3
  ( day3
  ) where

import Data.Bits (xor)
import Data.Char
import Data.List
import Foreign.Marshal.Utils (fromBool)

-- | convert array of bit into decimal value
fromBinary :: [Int] -> Int
fromBinary = foldl (\acc bit -> acc * 2 + bit) 0

-- | count number of occurence in list
count :: Char -> [Char] -> Int
count val = length . filter (val ==)

day3 :: IO ()
day3 = do
  values <- lines <$> readFile "input_day3.txt"
  let size = length $ head $ transpose values
      gamma = map (toValue . count '1') $ transpose values
      epsilon = map (`xor` 1) gamma :: [Int]
      toValue x = fromBool ((x * 2) >= size)
  print $ "day3 - part1 : " ++ show (fromBinary gamma * fromBinary epsilon)
  let o2Rating =
        fromBinary $ map digitToInt $ o2 values [0 .. length (head values) - 1]
      co2Rating =
        fromBinary $ map digitToInt $ co2 values [0 .. length (head values) - 1]
  print $ "day3 - part2 : " ++ show (o2Rating * co2Rating)

o2 :: [String] -> [Int] -> String
o2 [x] _ = x
o2 xs (y:ys) =
  let l = length xs
      toValue' x = fromBool ((x * 2) >= l)
      g = map (toValue' . count '1') $ transpose xs
   in o2 (filter (\x' -> x' !! y == intToDigit (g !! y)) xs) ys

co2 :: [String] -> [Int] -> String
co2 [x] _ = x
co2 xs (y:ys) =
  let l = length xs
      toValue' x = fromBool ((x * 2) > l)
      g = map (toValue' . count '0') $ transpose xs
   in co2 (filter (\x' -> x' !! y == intToDigit (g !! y)) xs) ys
