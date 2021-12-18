{-# LANGUAGE TupleSections #-}

import Data.Function
import qualified Data.IntMap as IM
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Map (Map)
import qualified Data.Map as M

type Grid = IntMap Int

type Pt = Int

conv :: (Int, Int) -> Pt
conv (x, y) = 10000 * x + y

from :: Pt -> (Int, Int)
from n = (n `div` 10000, n `mod` 10000)

toGrid :: [[Int]] -> Grid
toGrid = IM.fromList . concat . zipWith (\x l -> zipWith (curry (\((x, a), y) -> (conv (x, y), a))) (map (x,) l) [1 ..]) [1 ..]

toGrid2 = toGrid . embig

succW :: Int -> Int
succW n = max ((n + 1) `mod` 10) 1

embig l = concat $ take 5 $ iterate (map (map succW)) [concat $ take 5 (iterate (map succW) r) | r <- l]

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

type PQueue a = IntMap [a]

pminView :: PQueue a -> (a, PQueue a)
pminView p =
  let Just (l, p') = IM.minViewWithKey p
   in case l of
        (_, []) -> pminView p
        (k, x : xs) -> (x, if null xs then IM.delete k p' else IM.insert k xs p')

pins :: Int -> a -> PQueue a -> PQueue a
pins k x = IM.insertWith (++) k [x]

pempty :: PQueue a
pempty = IM.empty

neighbors :: Grid -> Int -> IntSet
neighbors g p = IS.filter (`IM.member` g) (IS.fromList (conv <$> [(x -1, y), (x, y + 1), (x + 1, y), (x, y -1)]))
  where
    (x, y) = from p

dijkstra :: IntMap Int -> Int -> (IntMap Int, IntMap Int)
dijkstra g s = step1 & step2 & step3
  where
    step1 = let c = IM.keysSet g; (a, b) = IS.foldl' go (mempty, mempty) c in (a, b, c)
    go (dv, pv) v = (dv', pv')
      where
        dv' = IM.insert v (maxBound :: Int) dv
        pv' = IM.insert v (conv (0, 0)) pv
    step2 (dv, pv, q) = (IM.insert s (conv (0, 0)) dv, pv, q)
    step3 (dv, pv, q)
      | IS.null q = (dv, pv)
      | otherwise = step3 (dv', pv', q')
      where
        q' = IS.delete u q
        u = nextVert dv
        (dv', pv') = IS.foldl' f (dv, pv) (IS.intersection q (neighbors g u))
          where
            f (dv, pv) v = (dv'', pv'')
              where
                alt = dv IM.! u + g IM.! v
                (dv'', pv'') = if alt < dv IM.! v then (IM.insert v alt dv, IM.insert v u pv) else (dv, pv)
        nextVert :: IntMap Int -> Int
        nextVert dv = v
          where
            v :: Int
            v = fst $ IS.foldl' (\(x, c) y -> if (dv IM.! y) < c then (y, dv IM.! y) else (x, c)) (undefined, maxBound :: Int) q

part1 (inp, n) = fst (dijkstra inp (conv (1, 1))) IM.! conv (n, n)

main = do
  let dayNumber = 15 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp' <- readFile dayFilename
  let x = map (map (read . pure)) . lines $ inp'
  let inp = toGrid x
  let inp2 = toGrid2 x
  let n = length (head $ lines $ inp')
  print (part1 (inp, n))
  print (part1 (inp2, n * 5))

-- -- defaultMain
-- --   [ bgroup
-- --       dayString
-- --       [ bench "part1" $ whnf part1 inp,
-- --         bench "part2" $ whnf part2 inp
-- --       ]
-- --   ]
