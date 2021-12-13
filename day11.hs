{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import Control.Applicative
import Control.Monad
import Criterion.Main
import Data.Bifunctor
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.Function
import qualified Data.Graph as G
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Text.ParserCombinators.ReadP as P

-- Start working down here
part1, part2 :: _ -> Int
part1 i = undefined
part2 i = undefined

type Grid = Map (Int, Int) Int

toGrid :: [[Int]] -> Grid
toGrid = M.fromList . concat . zipWith (\ x l -> zipWith (curry (\((x,a),y) -> ((x,y),a))) (map (x,) l) [1..]) [1..]

nbs (x,y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1), (x-1,y-1),(x-1,y+1),(x+1,y+1),(x+1,y-1)]

type Pt = (Int, Int)

-- tick :: Pt -> Grid -> (Grid, [Pt])
-- tick p g = mapAccumL _ (nbs p)
tick :: Grid -> ((Int, [Pt]), Grid)
tick g = M.mapAccumWithKey f (0, []) g
  where
    -- f :: [Pt] -> (Int, Int) -> Int -> ([Pt], Int)
    -- Flash the point and set it to 0
    f (s, l) p n | n >= 9 = ((s+1, p:l),0)
    f (s, l) p n = ((s,l),n+1)


propFlashes = until (null . snd . fst) comb
-- given neighbors of points that just flashed, return the grid and
-- the new neighbors that just flashed
-- return a tuple consisting of new points to flash and the modified grid
-- comb :: ([Pt], Grid) -> ([Pt], Grid)
comb ((s, fs), g) = foldl' f ((s,[]), g) fs
  where
    -- For each point to flash, consider its neighbors
    -- if the neighbor doesn't exist, ignore it.
    f ((s,l),g) p = foldl' (\((s,l),g') p -> maybe ((s,l),g') (f s l p g') (g' M.!? p)) ((s,l),g) ns
          where
            -- For a given neighbor,
            -- If it's 0, that means it was just flashed, don't adjust.
            f s l p g 0 = ((s, l),g)
            -- If its value is greater than or equal to 8, add it to
              -- the flash queue and insert 0 into that point for the
            -- next round.
            f s l p g n | n >= 9 = ((s+1,p:l), M.insert p 0 g)
            -- Otherwise increment its value
            f s l p g n = ((s,l), M.insert p (n+1) g)
            ns = nbs p

run = f . propFlashes . tick
  where
    f ((s,l),m) = (s,m)


countFlashes n g = go n g 0
  where
    go 0 g m = m
    go n g m = go (n-1) g' (m+m')
      where
        (m',g') = run g

firstSync g = go 0 0 g
  where
    go n 100 g = n
    go n _ g = go (n+1) m' g'
      where
        (m',g') = run g


grid2List g = [[g M.! (x,y) | y <- [1..w]] | x <- [1..w]]
  where
    w = round (sqrt (fromIntegral (M.size g)))

showGrid = unlines . map (concat . concatMap (map show . pure)) . grid2List

printGrid = putStrLn . showGrid
sg = printGrid
ex1 = M.fromList [((1,1),5),((1,2),4),((1,3),8),((1,4),3),((1,5),1),((1,6),4),((1,7),3),((1,8),2),((1,9),2),((1,10),3),((2,1),2),((2,2),7),((2,3),4),((2,4),5),((2,5),8),((2,6),5),((2,7),4),((2,8),7),((2,9),1),((2,10),1),((3,1),5),((3,2),2),((3,3),6),((3,4),4),((3,5),5),((3,6),5),((3,7),6),((3,8),1),((3,9),7),((3,10),3),((4,1),6),((4,2),1),((4,3),4),((4,4),1),((4,5),3),((4,6),3),((4,7),6),((4,8),1),((4,9),4),((4,10),6),((5,1),6),((5,2),3),((5,3),5),((5,4),7),((5,5),3),((5,6),8),((5,7),5),((5,8),4),((5,9),7),((5,10),8),((6,1),4),((6,2),1),((6,3),6),((6,4),7),((6,5),5),((6,6),2),((6,7),4),((6,8),6),((6,9),4),((6,10),5),((7,1),2),((7,2),1),((7,3),7),((7,4),6),((7,5),8),((7,6),4),((7,7),1),((7,8),7),((7,9),2),((7,10),1),((8,1),6),((8,2),8),((8,3),8),((8,4),2),((8,5),8),((8,6),8),((8,7),1),((8,8),1),((8,9),3),((8,10),4),((9,1),4),((9,2),8),((9,3),4),((9,4),6),((9,5),8),((9,6),4),((9,7),8),((9,8),5),((9,9),5),((9,10),4),((10,1),5),((10,2),2),((10,3),8),((10,4),3),((10,5),7),((10,6),5),((10,7),1),((10,8),5),((10,9),2),((10,10),6)]

ex2 = M.fromList [((1,1),1),((1,2),1),((1,3),1),((1,4),1),((1,5),1),((2,1),1),((2,2),9),((2,3),9),((2,4),9),((2,5),1),((3,1),1),((3,2),9),((3,3),1),((3,4),9),((3,5),1),((4,1),1),((4,2),9),((4,3),9),((4,4),9),((4,5),1),((5,1),1),((5,2),1),((5,3),1),((5,4),1),((5,5),1)]
nbsPow9Cnt (x,y) g = length $ (g M.!) <$> nbs (x,y)
-- | Apply a function @n@ times
apN :: Int -> (a -> a) -> a -> a
apN 0 f !x = x
apN !n f !x = apN (n - 1) f (f x)


main = do
  let dayNumber = 11 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- toGrid . map (map (read . pure)) . lines <$> readFile dayFilename
  print (countFlashes 100 inp)
  print (firstSync inp)
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]
