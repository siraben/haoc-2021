{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
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

-- valleyFind
type Grid = Map (Int, Int) Char

toGrid :: [String] -> Grid
toGrid g = M.fromList (concatMap dist (zip [1 ..] g))
  where
    dist (a, b) = zipWith (\x c -> ((a, x), c)) [1 ..] b

checkVal :: Grid -> (Int, Int) -> Bool
checkVal g p@(x, y)
  | M.notMember p g = False
  | otherwise =
    check (x -1, y) && check (x + 1, y) && check (x, y -1) && check (x, y + 1)
  where
    check p = maybe True (n <) (g M.!? p)
    n = g M.! p

-- expand wrt. Grid
expand g cs = S.union (S.unions (S.map nexts cs)) cs
  where
    nexts (x, y) = S.filter f (S.fromList [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)])
      where
        f (a, b) = maybe False (/= '9') (g M.!? (a, b))

part1 (g, w, h) = sum lvls
  where
    pts = [(x, y) | x <- [1 .. h], y <- [1 .. w], checkVal g (x, y)]
    lvls = map (\p -> 1 + read [g M.! p]) pts

part2 (g, pts) = product (take 3 (reverse (sort (map (length . p2) pts))))
  where
    p2 = fixedPoint (expand g) . S.singleton

main = do
  let dayNumber = 9 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let w = length (head inp)
  let h = length inp
  let g = toGrid inp
  let pts = [(x, y) | x <- [1 .. h], y <- [1 .. w], checkVal g (x, y)]
  print (part1 (g, w, h))
  print (part2 (g, pts))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 (g, w, h),
          bench "part2" $ whnf part2 (g, pts)
        ]
    ]
