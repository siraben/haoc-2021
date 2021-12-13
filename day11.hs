{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.Foldable
import qualified Data.IntMap as IM
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List
import Data.Maybe
import Control.Monad

type Pt = Int

type PtSet = IntSet

type Grid = IntMap Int

conv :: (Int, Int) -> Pt
conv (x, y) = 100 * x + y

from :: Pt -> (Int, Int)
from n = (n `div` 100, n `mod` 100)

toGrid :: [[Int]] -> Grid
toGrid = IM.fromList . concat . zipWith (\x l -> zipWith (curry (\((x, a), y) -> (conv (x, y), a))) (map (x,) l) [1 ..]) [1 ..]

nbs :: Pt -> [Pt]
nbs p = [p -101, p -100, p -99, p -1, p + 1, p + 99, p + 100, p + 101]

tick :: Grid -> ((Int, PtSet), Grid)
tick = IM.mapAccumWithKey f (0, S.empty)
  where
    f (s, l) p n | n >= 9 = ((s + 1, S.insert p l), 0)
    f (s, l) p n = ((s, l), n + 1)

propFlashes :: ((Int, PtSet), IntMap Int) -> ((Int, PtSet), IntMap Int)
propFlashes = until (S.null . snd . fst) comb

comb :: ((Int, PtSet), IntMap Int) -> ((Int, PtSet), IntMap Int)
comb ((s, fs), g) = let (s', g') = S.foldl' f (S.empty, g) fs in ((s + S.size s', s'), g')
  where
    -- For each point to flash, consider its neighbors
    -- if the neighbor doesn't exist, ignore it.
    f (l, g) p = foldl' (\(l, g') p -> maybe (l, g') (f l p g') (g' IM.!? p)) (l, g) ns
      where
        -- For a given neighbor,
        -- If it's 0, that means it was just flashed, don't adjust.
        f l p g 0 = (l, g)
        -- If its value is greater than or equal to 9, add it to
        -- the flash queue and insert 0 into that point for the
        -- next round.
        f l p g n | n >= 9 = (S.insert p l, IM.insert p 0 g)
        -- Otherwise increment its value
        f l p g n = (l, IM.insert p (n + 1) g)
        ns = nbs p

run :: Grid -> (Int, IntMap Int)
run = f . propFlashes . tick
  where
    f ((s, l), m) = (s, m)

runs :: IntMap Int -> [Int]
runs g = l 0 g
  where
    l x g = x : l (x+m') g'
      where
        (m',g') = run g

part1 :: IntMap Int -> Int
part1 = (!! 100) . runs

part2 :: IntMap Int -> Int
part2 = succ . fromJust . findIndex (== -100) . (zipWith (-) `ap` tail) . runs

main = do
  let dayNumber = 11 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- toGrid . map (map (read . pure)) . lines <$> readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
