{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import Data.Char
import Data.Foldable
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Text.ParserCombinators.ReadP as P

part1 = IS.size . snd . foldl' draw (mempty, mempty)

part2 = IS.size . snd . foldl' draw2 (mempty, mempty)

pp = do
  (a :: (Int, Int)) <- pair
  P.skipSpaces
  P.string "->"
  P.skipSpaces
  (b :: (Int, Int)) <- pair
  P.char '\n'
  pure (a, b)
  where
    pair = (,) <$> P.readS_to_P reads <*> (P.string "," *> P.readS_to_P reads)

type Grid = (IntSet, IntSet)

type Line = ((Int, Int), (Int, Int))

conv :: (Int, Int) -> Int
conv (a, b) = a * 1000 + b

ins :: Int -> Grid -> Grid
ins i (a, b)
  | IS.member i a = (a, IS.insert i b)
  | otherwise = (IS.insert i a, b)

draw :: Grid -> Line -> Grid
draw g ((a, b), (c, d))
  | a == c = foldl' (\g p -> ins (conv p) g) g [(a, x) | x <- [(min b d) .. (max b d)]]
  | b == d = foldl' (\g p -> ins (conv p) g) g [(x, b) | x <- [(min a c) .. (max a c)]]
  | otherwise = g

draw2 :: Grid -> Line -> Grid
draw2 g ((a, b), (c, d))
  | a == c = foldl' (\g p -> ins (conv p) g) g [(a, x) | x <- [(min b d) .. (max b d)]]
  | b == d = foldl' (\g p -> ins (conv p) g) g [(x, b) | x <- [(min a c) .. (max a c)]]
  | abs (a - c) == abs (b - d) = foldl' (\g p -> ins (conv p) g) g (zip (enum a m1 c) (enum b m2 d))
  | otherwise = g
  where
    enum l s c
      | l == c = [l]
      | otherwise = l : enum (l + s) s c
    m1 = if a > c then -1 else 1
    m2 = if b > d then -1 else 1

main = do
  let dayNumber = 5 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let (inp', _) = last (P.readP_to_S (P.many1 pp) inp)
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
