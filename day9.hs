{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

type Pt = (Int, Int)

type Grid = IntMap Int

to :: Pt -> Int
to (x, y) = 1000 * x + y

from :: Int -> Pt
from n = (n `div` 1000, n `mod` 1000)

-- neighbors
nbs :: Int -> [Int]
nbs n = to <$> [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    (x, y) = from n

toGrid :: [String] -> Grid
toGrid g = IM.fromList (concatMap dist (zip [0 ..] g))
  where
    dist (a, b) = zipWith (\x c -> (to (a, x), read [c])) [0 ..] b

checkVal :: Grid -> Grid
checkVal g = IM.filterWithKey f g
  where
    f p n = all (check n) (nbs p)
    check n p = maybe True (n <) (g IM.!? p)

-- expand wrt. Grid and visited set
expand :: Grid -> IntSet -> IntSet -> Int
expand g cs vs | IS.null cs = IS.size vs
expand g cs vs = expand g cs' (vs `IS.union` cs')
  where
    cs' = IS.foldl' (\y x -> y `IS.union` nexts x) IS.empty cs IS.\\ vs
    nexts n = IS.filter f (IS.fromList (nbs n))
      where
        f n = maybe False (/= 9) (g IM.!? n)

part1 :: (Grid, Int, Int) -> Int
part1 (g, w, h) = IM.size pts + sum pts
  where
    pts = checkVal g

-- Given a grid and its basin points
part2 :: (Grid, Grid) -> Int
part2 (g, pts) = a * b * c
  where
    basins = IS.map p2 pts'
    Just (a, b') = IS.maxView basins
    Just (b, b'') = IS.maxView b'
    Just (c, _) = IS.maxView b''
    pts' = IM.keysSet pts
    p2 x = expand g (IS.singleton x) IS.empty

main = do
  let dayNumber = 9 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let w = length (head inp)
  let h = length inp
  let g = toGrid inp
  let pts = checkVal g
  print (part1 (g, w, h))
  print (part2 (g, pts))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 (g, w, h),
          bench "part2" $ whnf part2 (g, pts)
        ]
    ]
