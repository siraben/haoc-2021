{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Criterion.Main
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

dup a = (a, a)

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

-- most common and least common bits
toBinNum = foldl' f 0
  where
    f n '1' = n * 2 + 1
    f n '0' = n * 2

mcblcb l = (f p, g p)
  where
    f (a, b)
      | a == b = '1'
      | a < b = '1'
      | a > b = '0'
    g (a, b)
      | a == b = '0'
      | a < b = '0'
      | a > b = '1'
    p = comp l

mcb = fst . mcblcb

lcb = snd . mcblcb

comp = f' (0, 0)
  where
    f' (a, b) [] = (a, b)
    f' (a, b) ('0' : xs) = f' (a + 1, b) xs
    f' (a, b) ('1' : xs) = f' (a, b + 1) xs

part1 inp' = toBinNum x * toBinNum y
  where
    (x, y) = unzip $ map mcblcb inp'

part2 inp = h (f1 y) * h (f2 y)
  where
    g f [x] = [x]
    g f l = zip (tail <$> bs') ns'
      where
        bs = head . fst <$> l
        (bs', ns') = unzip (filter ((== b) . head . fst) l)
        b = f bs
    h = toBinNum . snd . head
    f1 = fixedPoint (g mcb)
    f2 = fixedPoint (g lcb)
    y = map dup inp

main = do
  let dayNumber = 3 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = transpose inp
  print (part1 inp')
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
