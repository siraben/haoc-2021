{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

type Pt = (Int, Int)

type Grid = Map Pt Int

toGrid :: [String] -> Grid
toGrid g = M.fromList (concatMap dist (zip [1 ..] g))
  where
    dist (a, b) = zipWith (\x c -> ((a, x), read [c])) [1 ..] b

checkVal :: Grid -> Pt -> Bool
checkVal g p@(x, y)
  | M.notMember p g = False
  | otherwise =
    all check [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    check p = maybe True (n <) (g M.!? p)
    n = g M.! p

checkVal' :: Grid -> Grid
checkVal' g = M.filterWithKey f g
  where
    f (x, y) n = all (check n) [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
    check n p = maybe True (n <) (g M.!? p)

-- expand wrt. Grid and visited set
expand :: Grid -> Set Pt -> Set Pt -> Int
expand g cs vs | S.null cs = S.size vs
expand g cs vs = expand g (cs' S.\\ vs) (cs' `S.union` vs) -- S.union (S.unions (S.map nexts cs)) cs
  where
    cs' = S.unions (S.map nexts cs)
    nexts (x, y) = S.filter f (S.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])
      where
        f (a, b) = maybe False (/= 9) (g M.!? (a, b))

part1 :: (Grid, Int, Int) -> Int
part1 (g, w, h) = M.size pts + sum pts
  where
    pts = checkVal' g

-- Given a grid and its basin points
part2 :: (Grid, Grid) -> Int
part2 (g, pts) = a * b * c
  where
    basins = S.map p2 pts'
    Just (a, b') = S.maxView basins
    Just (b, b'') = S.maxView b'
    Just (c, _) = S.maxView b''
    pts' = M.keysSet pts
    p2 x = expand g (S.singleton x) S.empty

main = do
  let dayNumber = 9 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let w = length (head inp)
  let h = length inp
  let g = toGrid inp
  let pts = checkVal' g
  print (part1 (g, w, h))
  print (part2 (g, pts))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 (g, w, h),
          bench "part2" $ whnf part2 (g, pts)
        ]
    ]
