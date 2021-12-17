{-# LANGUAGE TupleSections #-}

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Function

toGrid :: [[Int]] -> Grid
toGrid = M.fromList . concat . zipWith (\x l -> zipWith (curry (\((x, a), y) -> ((x, y), a))) (map (x,) l) [1 ..]) [1 ..]

{-
 1  function Dijkstra(Graph, source):
 2
 3      create vertex set Q
 4
 5      for each vertex v in Graph:
 6          dist[v] ← INFINITY
 7          prev[v] ← UNDEFINED
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
20                  prev[v] ← u
21
22      return dist[], prev[]
-}

type Grid = Map (Int, Int) Int

neighbors :: Grid -> (Int, Int) -> Set (Int, Int)
neighbors g (x, y) = S.filter (`M.member` g) (S.fromList [(x -1, y), (x, y + 1), (x + 1, y), (x, y -1)])

dijkstra :: Map (Int, Int) Int -> (Int, Int) -> (Map (Int, Int) Int, Map (Int, Int) (Int, Int))
dijkstra g s = step1 & step2 & step3
  where
    step1 = let c = M.keysSet g; (a, b) = foldl' go (M.empty, M.empty) c in (a, b, c)
    go (dv, pv) v = (dv', pv')
      where
        dv' = M.insert v (maxBound :: Int) dv
        pv' = M.insert v (0, 0) pv
    step2 (dv, pv, q) = (M.insert s 0 dv, pv, q)
    step3 (dv, pv, q)
      | S.null q = (dv, pv)
      | otherwise = step3 (dv', pv', q')
      where
        q' = S.delete u q
        u = nextVert dv
        (dv', pv') = S.foldl' f (dv, pv) (S.intersection q (neighbors g u))
          where
            f (dv, pv) v = (dv'', pv'')
              where
                alt = dv M.! u + g M.! v
                (dv'', pv'') = if alt < dv M.! v then (M.insert v alt dv, M.insert v u pv) else (dv, pv)
        nextVert :: Map (Int, Int) Int -> (Int, Int)
        nextVert dv = v
          where
            v :: (Int, Int)
            v = minimumBy (\x y -> compare (dv M.! x) (dv M.! y)) q

part1 (inp, n) = fst (dijkstra inp (1, 1)) M.! (n, n)

main = do
  let dayNumber = 15 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp' <- readFile dayFilename
  let inp = toGrid . map (map (read . pure)) . lines $ inp'
  let n = length (head $ lines $ inp')
  print (part1 (inp, n))

-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]
