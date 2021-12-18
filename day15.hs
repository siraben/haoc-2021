{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

type Grid = IntMap Int

type Pt = Int

factor :: Int
factor = 10000

conv :: (Int, Int) -> Pt
conv (x, y) = factor * x + y

from :: Pt -> (Int, Int)
from n = (n `div` factor, n `mod` factor)

toGrid :: [[Int]] -> Grid
toGrid = IM.fromList . concat . zipWith (\x l -> zipWith (curry (\((x, a), y) -> (conv (x, y), a))) (map (x,) l) [1 ..]) [1 ..]

toGrid2 :: [[Int]] -> Grid
toGrid2 = toGrid . embig

succW :: Int -> Int
succW n = max ((n + 1) `mod` 10) 1

embig :: [[Int]] -> [[Int]]
embig = concat . take 5 . iterate (map (map succW)) . map (concat . take 5 . iterate (map succW))

{-
 1  function Dijkstra(Graph, source):
 2
 3      create vertex set Q
 4
 5      for each vertex v in Graph:
 6          dist[v] ← INFINITY
 7          # prev[v] ← UNDEFINED
 8          add v to Q
 9      dist[source] ← 0
10
11      while Q is not empty:
12          u ← vertex in Q with min dist[u]
13
14          remove u from Q
15
16          for each neighbor v of u still in Q:
17              alt ← dist[u] + length(u, v)
18              if alt < dist[v]:
19                  dist[v] ← alt
20                  # prev[v] ← u
21
22      return dist[]
-}

type PQueue a = IntMap [a]

pminView :: PQueue a -> ((Int, a), PQueue a)
pminView p =
  let Just (l, p') = IM.minViewWithKey p
   in case l of
        (_, []) -> pminView p
        (k, x : xs) -> ((k, x), if null xs then IM.delete k p' else IM.insert k xs p')

pins :: Int -> a -> PQueue a -> PQueue a
pins k x = IM.insertWith (++) k [x]

pempty :: PQueue a
pempty = IM.empty

neighbors :: Int -> [Int]
neighbors p = conv <$> [(x -1, y), (x, y + 1), (x + 1, y), (x, y -1)]
  where
    (x, y) = from p

dijkstra :: Grid -> Int -> IntMap Int
dijkstra g s = step1 & step2
  where
    step1 = (IM.map (const (maxBound :: Int)) g, IM.keysSet g, pins 0 s pempty)
    step2 (dv, unseen, q)
      | IM.null q = dv
      | otherwise = step2 (dv', unseen', q'')
      where
        -- u is the closest vertex to visit
        ((d, u), q') = pminView q
        unseen' = IS.delete u unseen
        -- for all neighbors of u, visit it and thread the distance map and priority queue
        (dv', q'') = foldl' visit (dv, q') (neighbors u)
          where
            -- to visit a point p, make sure it's in the graph
            visit (dv, q') p
              | p `IM.member` g = (dv'', q'')
              | otherwise = (dv, q')
              where
                -- compute the new distance to p (store into alt) and
                -- compare against the previous distance
                alt = d + g IM.! p
                -- if it's better then update distance for p in dv to be alt
                (dv'', q'') = if alt < dv IM.! p then (IM.insert p alt dv, pins alt p q') else (dv, q')

part1 :: (Grid, Int) -> Int
part1 (inp, n) = dijkstra inp (conv (1, 1)) IM.! conv (n, n)

part2 :: (Grid, Int) -> Int
part2 = part1

main :: IO ()
main = do
  let dayNumber = 15 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp' <- readFile dayFilename
  let x = map (map (read . pure)) . lines $ inp'
  let inp = toGrid x
  let inp2 = toGrid2 x
  let n = length (head $ lines inp')
  print (part1 (inp, n))
  print (part1 (inp2, n * 5))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 (inp, n),
          bench "part2" $ whnf part2 (inp2, n * 5)
        ]
    ]
