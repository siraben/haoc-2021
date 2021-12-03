{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Monad
import Criterion.Main
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

dup a = (a, a)

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

-- Start working down here

-- most common bit
mcb s = fst $ maximumBy (compare `on` snd) (M.toAscList (freqs s))

lcb s = fst $ minimumBy (compare `on` snd) (M.toAscList (freqs s))

toBinNum = foldl' f 0
  where
    f n '1' = n * 2 + 1
    f n '0' = n * 2

part1 inp' = (toBinNum x * toBinNum y)
  where
    f s = (mcb s, lcb s)
    (x, y) = unzip $ map f inp'

part2 inp = toBinNum (snd (head (f1 y))) * toBinNum (snd (head (f2 y)))
  where
    g f [x] = [x]
    g f l = zip (tail <$> bs') ns'
      where
        bs = head . fst <$> l
        (bs', ns') = unzip (filter ((== b) . head . fst) l)
        b = f bs

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
