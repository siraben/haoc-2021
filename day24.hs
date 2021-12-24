{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.IntSet as IS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Parsec.Combinator as P
import Control.Monad.State
import Data.Functor

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


data Instr a = Inp a
             | Add a (Either a Int)
             | Mul a (Either a Int)
             | Div a (Either a Int)
             | Mod a (Either a Int)
             | Eql a (Either a Int)
             deriving (Show, Eq)

type Pgrm a = [Instr a]

interp :: Ord a => Pgrm a -> [Int] -> State (Map a Int) ()
interp [] i = pure ()
interp (x:xs) i = do
               l' <- f x i
               interp xs l'
  where
    g x = do
      n <- gets (M.!? x)
      case n of
        Nothing -> modify (M.insert x 0) $> 0
        Just a -> pure a

    p a x = modify (M.insert a x)
    binop f a b i = do
      x <- g a
      y <- case b of
            Left b' -> g b'
            Right n -> pure n
      p a (f x y)
      pure i
    f :: Ord a => Instr a -> [Int] -> State (Map a Int) [Int]
    f (Inp a) (x:xs) = modify (M.insert a x) $> xs
    f (Add a b) i = binop (+) a b i
    f (Mul a b) i = binop (*) a b i
    f (Div a b) i = binop (\x y -> floor (fromIntegral x / fromIntegral y)) a b i
    f (Mod a b) i = binop mod a b i
    f (Eql a b) i = binop (\x y -> fromEnum (x == y)) a b i

numorId s = case s of
              [] -> undefined
              (x:xs) -> if isNumber x then Right (read @Int s) else Left s

pl ("inp":x:_) = Inp x
pl ["add", x, y] = Add x (numorId y)
pl ["mul", x, y] = Mul x (numorId y)
pl ["div", x, y] = Div x (numorId y)
pl ["mod", x, y] = Mod x (numorId y)
pl ["eql", x, y] = Eql x (numorId y)
pl _ = undefined

digits n = go n []
  where
    go 0 l = l
    go n l = go (n `div` 10) ((n `mod` 10):l)

isValid inp n = execState (interp inp n) M.empty M.! "z" == 0

ds = [9,8..1]

candidates :: [[Int]]
candidates = [[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]
             | a <- ds
             , b <- ds
             , c <- ds
             , d <- ds
             , e <- ds
             , f <- ds
             , g <- ds
             , h <- ds
             , i <- ds
             , j <- ds
             , k <- ds
             , l <- ds
             , m <- ds
             , n <- ds
             , o <- ds
             ]
main = do
  let dayNumber = 24 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map (pl . words) . lines <$> readFile dayFilename
  let n = digits 13579246899999
  print (filter (isValid inp) candidates)
  -- print (isValid inp n)
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]
