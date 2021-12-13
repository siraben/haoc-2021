{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M

type Grid = Map (Int, Int) Int

toGrid :: [[Int]] -> Grid
toGrid = M.fromList . concat . zipWith (\x l -> zipWith (curry (\((x, a), y) -> ((x, y), a))) (map (x,) l) [1 ..]) [1 ..]

nbs (x, y) = [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1], (a, b) /= (0, 0)]

type Pt = (Int, Int)

tick :: Grid -> ((Int, [Pt]), Grid)
tick = M.mapAccumWithKey f (0, [])
  where
    f (s, l) p n | n >= 9 = ((s + 1, p : l), 0)
    f (s, l) p n = ((s, l), n + 1)

propFlashes = until (null . snd . fst) comb

comb ((s, fs), g) = foldl' f ((s, []), g) fs
  where
    -- For each point to flash, consider its neighbors
    -- if the neighbor doesn't exist, ignore it.
    f ((s, l), g) p = foldl' (\((s, l), g') p -> maybe ((s, l), g') (f s l p g') (g' M.!? p)) ((s, l), g) ns
      where
        -- For a given neighbor,
        -- If it's 0, that means it was just flashed, don't adjust.
        f s l p g 0 = ((s, l), g)
        -- If its value is greater than or equal to 9, add it to
        -- the flash queue and insert 0 into that point for the
        -- next round.
        f s l p g n | n >= 9 = ((s + 1, p : l), M.insert p 0 g)
        -- Otherwise increment its value
        f s l p g n = ((s, l), M.insert p (n + 1) g)
        ns = nbs p

run = f . propFlashes . tick
  where
    f ((s, l), m) = (s, m)

countFlashes n g = go n g 0
  where
    go 0 g m = m
    go n g m = go (n -1) g' (m + m')
      where
        (m', g') = run g

firstSync g = go 0 0 g
  where
    go n 100 g = n
    go n _ g = go (n + 1) m' g'
      where
        (m', g') = run g

part1 = countFlashes 100

part2 = firstSync

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
