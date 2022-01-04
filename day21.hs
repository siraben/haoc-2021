{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import Control.Applicative hiding ((<|>))
import Control.Monad
import Criterion.Main
import Control.Arrow
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.Function
import qualified Data.Graph as G
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Parsec.Combinator as P

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- ByteString splitOn
splitOn' :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOn' del bs = go bs
  where
    n = B.length del
    go bs = case B.breakSubstring del bs of
      (ls, rest) ->
        if B.null rest
          then ls : mempty
          else ls : splitOn' del (B.drop n rest)

-- Useful functions
findFirst :: Foldable f => (a -> Bool) -> f a -> a
findFirst f = fromJust . find f

slidingWindows :: Int -> [Int] -> [[Int]]
slidingWindows n l = take n <$> tails l

-- | Compute the average of the elements of a container.
avg :: (Integral c, Foldable t) => t c -> c
avg = uncurry div . foldl' (\(s, l) x -> (x + s, succ l)) (0, 0)

swap (a, b) = (b, a)

dup a = (a, a)

-- | Count the number of items in a container where the predicate is true.
countIf :: Foldable f => (a -> Bool) -> f a -> Int
countIf p = length . filter p . toList

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | map a function over elems satisfying a predicate
mapIf p f = foldl' (\xs x -> if p x then f x : xs else x : xs) []

-- | Set the element at index @n@ to @x@
setAt n x = (\(l, r) -> l ++ x : tail r) . splitAt n

-- | Like @findIndices@ but also return the element found for each index
findIndicesElem :: Foldable t => (a -> Bool) -> t a -> [(a, Int)]
findIndicesElem p = fst . foldl' go ([], 0 :: Int)
  where
    go (l, n) x
      | p x = ((x, n) : l, n + 1)
      | otherwise = (l, n + 1)

-- | Perturb a list's elements satisfying a predicate with a function
pertubationsBy :: (a -> Bool) -> (a -> a) -> [a] -> [[a]]
pertubationsBy p f l = [setAt n (f x) l | (x, n) <- findIndicesElem p l]

-- | Unconditional pertubation
pertubations :: (a -> a) -> [a] -> [[a]]
pertubations = pertubationsBy (const True)

-- | Generate all the segments of a list, O(n^2)
segs :: [a] -> [[a]]
segs = concatMap tails . inits

-- | Repeat a function until you get the same result twice.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
      | x == y = x
      | otherwise = go y
      where
        y = f x

-- | Apply a function @n@ times
apN :: Int -> (a -> a) -> a -> a
apN 0 f !x = x
apN !n f !x = apN (n - 1) f (f x)

enum :: (Eq t, Num t) => t -> t -> t -> [t]
enum l s c
  | l == c = [l]
  | otherwise = l : enum (l + s) s c

range :: (Num a, Ord a) => a -> a -> [a]
range a b = enum a (signum (b - a)) b

-- Start working down here
part1 i = undefined
part2 i = undefined

-- moveF n p = max ((n + p) `mod` 11) 1
moveF n p = (n + p) `mod` 10

brk3 [] = [[]]
brk3 (a:b:c:l) = [a,b,c]:brk3 l

p1S = 3
p2S = 7
moves = map sum $ brk3 (cycle [1..100])
alt (x:y:r) = (x,y):alt r

unalt ((x,y):r) = x:y:unalt r
p1Moves = map fst $ alt moves
p2Moves = map snd $ alt moves

posScore = nub [x+y+z|x<-[1..3],y<-[1..3],z<-[1..3]]

solve = res
  where
    res = if p1Won then b*f else e*c
    timesRolled = if p1Won then c else f
    lps = if p1Won then (let (a,x,c) = l2 !! (timesRolled `div ` 6) in x) else (let (a,x,c) = l !! (timesRolled `div ` 6) in x)
    p1Won = c <= f
    p2Won = c > f
    ((a,b,c),(d,e,f)) = findFirst (\((a,b,c),(d,e,f)) -> max b e >= 1000) (zip l l2)
    g = \(p,s,c) n -> let m = moveF n p in (m,m+s+1,c+6)
    l = tail $ scanl' g (p1S,0,-3) p1Moves
    l2 = tail $ scanl' g (p2S,0,0) p2Moves

solve' = fst l
  where
    xx = \(p,s,c) -> [ (m,m+s+1,c+6)| x<- posScore,let m = moveF x p]
    l = until (null . snd) (\(n,g) -> let (a,b) = partition (\(a,b,c) -> b >= 21) g in (n + length a, concatMap xx b)) (0, [(p1S,0,-3)])
    -- l2 = tail $ scanl' (\n _ -> concatMap g n) [(p2S,0,0)] p2Moves


-- player 1 pos and score
-- player 2 pos and score
--type Universe = [(Int, Int)]
-- runQ :: Universe -> Universe

-- the boolean indicates whether or not its their turn
-- runQ p1S p2S = go [(p1S, 0,True),(p2S, 0,False)] 0 0
--   where
--     go l =
--       where
--         (finished, ) = partition ()
-- runQ :: [_] -> [_]
-- runQ l = do
--   ((p1,s1),(p2,s2)) <- l
--   p <- posScore
--   p' <- posScore
--   let m1 = moveF p1 p
--   let m2 = moveF p2 p'
  
--   pure [m1,m2]
  -- let (p1', s1') = 
  -- pure ((p1,s2))
-- univ (p1,s1) (p2,s2) us | (max s1 s2) >= 21 = n
--                         | otherwise =

main = do
  let dayNumber = 21 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  print (take 10 inp)
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]
