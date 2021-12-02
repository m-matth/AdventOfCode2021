{-# LANGUAGE FlexibleInstances #-}

module Day2
  ( day2
  ) where

import Text.Read

data Command a
  = Forward a
  | Up a
  | Down a
  | Depth a
  | Aim a
  deriving (Show, Eq)

instance Read (Command Int) where
  readsPrec _ input =
    case words input of
      ["forward", val] -> [(Forward (read val :: Int), "")]
      ["up", val] -> [(Up (read val :: Int), "")]
      ["down", val] -> [(Down (read val :: Int), "")]
      x -> [(Depth 0, "")]
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Functor Command where
  fmap g (Forward a) = Forward (g a)
  fmap g (Up a) = Depth (g a)
  fmap g (Down a) = Depth (g a)
  fmap g (Depth a) = Depth (g a)
  fmap g (Aim a) = Aim (g a)

parseInput :: String -> [Command Int]
parseInput s = map (\x -> read x :: Command Int) (lines s)

position ::
     (Num a) => (Command a, Command a) -> Command a -> (Command a, Command a)
position (Forward x, u) f@(Forward x') = (fmap (+ x) f, u)
position (f, u) (Up y') = (f, fmap (\y'' -> y'' - y') u)
position (f, u) (Down y') = (f, fmap (+ y') u)
position x y = x

type Commands a = (Command a, Command a, Command a)

position' :: (Num a) => Commands a -> Command a -> Commands a
position' (Forward x, d, Aim a) f@(Forward x') =
  (fmap (+ x) f, fmap (+ (x' * a)) d, Aim a)
position' (f, u, a) (Up y') = (f, u, fmap (flip (-) y') a)
position' (f, u, a) (Down y') = (f, u, fmap (+ y') a)
position' x y = x

day2 :: IO ()
day2 = do
  values <- parseInput <$> readFile "input_day2.txt"
  print $
    "day2 - part1 : " ++
    show ((\(Forward x, Depth y) -> x * y) (result1 values))
  print $
    "day2 - part2 : " ++
    show ((\(Forward x, Depth y, _) -> x * y) (result2 values))
  where
    result1 = foldl position (Forward 0, Depth 0)
    result2 = foldl position' (Forward 0, Depth 0, Aim 0)
