{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -fdefer-typed-holes -fno-warn-unused-imports #-}

import qualified Control.Applicative as A
import Control.Monad
import Data.Either
import Control.Arrow ((>>>), (***), (&&&))
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
import Text.ParserCombinators.Parsec
import Data.Ord

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

-- | Signum function
sgn :: (Ord a, Num a) => a -> a
sgn x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
  | otherwise = undefined

enum :: (Eq t, Num t) => t -> t -> t -> [t]
enum l s c
  | l == c = [l]
  | otherwise = l : enum (l + s) s c

range :: (Num a, Ord a) => a -> a -> [a]
range a b = enum a (sgn (b - a)) b

-- Start working down here
part1, part2 :: _ -> Int
part1 i = undefined
part2 i = undefined

data T = T (Either Int T) (Either Int T) deriving (Show, Eq)

lb = char '['

rb = char ']'

pp :: Parser T
pp = T <$> (lb *> comp) <*> (char ',' *> comp) <* rb
  where
    comp = Left <$> nat <|> Right <$> pp

-- split :: Int -> T
-- split n = T (Left a) (Left (n - a))
--   where
--     a = n `div` 2

-- -- reduce :: T -> T
-- reduce (Left n) = Right (split n)

recur :: (Int -> t2) -> (Int -> t2 -> t2 -> t2) -> T -> t2
recur f g = go 0
  where
    go n (T a b) = g n (either f (go n') a) (either f (go n') b)
      where
        n' = n + 1

recur'' f g = go
  where
    go (Leaf a) = f a
    go (Fork a b) = g (go a) (go b)

-- recur :: (Int -> t2) -> (Int -> t2 -> t2 -> t2) -> T -> t2
-- recur f g x = go 0 x (top x)
--   where
--     go 
--     go n (Fork a b) l = g n (either (f (left l)) (\x -> go n' x (left l)) a) (either (f (right l)) (\x -> go n' x (right l)) b)
--       where
--         n' = n + 1

recur' f g x = go x id
  where
    go (Leaf a) l = f l
    go (Fork a b) l = g (go a (left . l)) (go b (right . l))

lvl1 (Fork (Leaf _) (Leaf _),_) = True
lvl1 _ = False
findEx tt = group ((map up . filter (\y -> depth y > 4) . (\t -> recur' (\p -> [p (top t)]) (++) t)) tt) & map head & find lvl1

findEx' tt = group ((map up . sortBy (compare `on` depth) . filter (\y -> depth y >= 4) . (\t -> recur' (\p -> [p (top t)]) (++) t)) tt) & map head
-- findEx tt = group ((map up . filter (\y -> depth y >= 4 ) . (\t -> recur' (\p -> [p (top t)]) (++) t)) tt) & head & head
depth :: Loc a -> Int
depth (_,l) = go l
  where
    go Top = 0
    go (L c _) = 1 + go c
    go (R _ c) = 1 + go c

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

Right ex1 = parse pp "" "[[[[[9,8],1],2],3],4]"

ex1' = tToTree ex1

test = recur show (\n x y -> (if n >= 4 then "0" else "[" ++ x ++ "," ++ y ++ "]"))

rend = recur show (\n x y -> "[" ++ x ++ "," ++ y ++ "]")
tToTree = recur Leaf (const Fork)

data Tree a = Fork (Tree a) (Tree a) | Leaf a deriving (Show, Eq, Ord)

type Loc a = (Tree a, Cxt a)

data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a) deriving (Show, Eq, Ord)

left :: Loc a -> Loc a
left (Fork l r, c) = (l, L c r)

right :: Loc a -> Loc a
right (Fork l r, c) = (r, R l c)

-- mleft :: Loc a -> Loc a
mleft (Fork l r, c) = Just (l, L c r)
mleft _ = Nothing

-- mright :: Loc a -> Loc a
mright (Fork l r, c) = Just (r, R l c)
mright _ = Nothing

top :: Tree a -> Loc a
top t = (t, Top)

up :: Loc a -> Loc a
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)

upmost :: Loc a -> Loc a
upmost l@(t, Top) = l
upmost l = upmost (up l)

leftmost :: Loc a -> Loc a
leftmost l@(Leaf _, _) = l
leftmost l = leftmost (left l)

rightmost :: Loc a -> Loc a
rightmost l@(Leaf _, _) = l
rightmost l = rightmost (right l)

-- mleftmost :: Loc a -> Loc a
mleftmost l@(Leaf _, _) = Just l
-- mleftmost l@(Fork _ _, _) = Just l
mleftmost l = mleftmost (left l)

firstRegLeft r@(_, R _ _) = Just (rightmost (left (up r)))
firstRegLeft r@(_, L _ _) = firstRegLeft (followL r)
firstRegLeft r@(_, _) = Nothing

firstRegRight r@(_, L _ _) = Just (leftmost (right (up r)))
firstRegRight r@(_, R _ _) = firstRegRight (followR r)
firstRegRight r@(_, _) = Nothing

followR r@(_, R _ _) = followR (up r)
followR r = r

followL r@(_, L _ _) = followL (up r)
followL r = r
-- first regular number on the right
-- if we were on the right, go up and recur
frr r@(_, R _ _) = frr (up r)
-- frl r@(_, R _ _) = frl (up r)

addT = Fork


modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t, c) f = (f t, c)

-- modify' :: Loc a -> (Tree a -> Tree a) -> Loc a
modify' (t, c) f = ((f t, c), t)

maxT :: Tree Int -> Tree Int -> Tree Int
maxT (Leaf a) (Leaf b) = Leaf (max a b)
maxT (Fork l r) (Fork l' r') = Fork (maxT l l') (maxT r r')
maxT _ _ = undefined

solve = fixedPoint explode

addS x y = solve (Fork x y)
explode :: Tree Int -> Tree Int
explode ff = ff & bb (\x -> x & f &&& g & uncurry maxT & doSplit)
  where
    bb f t = case findEx t of
             Nothing -> t
             Just r -> f (modify' r (const (Leaf 0)))
    f (l,Fork (Leaf a) (Leaf b)) = l & firstRegRight & maybe (l & upmost & fst) (\x -> x & (`modify` (\(Leaf n) -> Leaf (n + b))) & upmost & fst)
    g (l,Fork (Leaf a) (Leaf b)) = l & firstRegLeft & maybe (l & upmost & fst) (\x -> x & (`modify` (\(Leaf n) -> Leaf (n + a))) & upmost & fst)


-- explode' :: Tree Int -> Tree Int
explode' ff = ff & bb (\x -> x & f &&& g) -- & uncurry maxT & doSplit)
  where
    bb f t = case findEx t of
             Nothing -> (t,t)
             Just r -> f (modify' r (const (Leaf 0)))
    f (l,Fork (Leaf a) (Leaf b)) = l & firstRegRight & maybe (l & upmost & fst) (\x -> x & (`modify` (\(Leaf n) -> Leaf (n + b))) & upmost & fst)
    g (l,Fork (Leaf a) (Leaf b)) = l & firstRegLeft & maybe (l & upmost & fst) (\x -> x & (`modify` (\(Leaf n) -> Leaf (n + a))) & upmost & fst)

doSplit t = fst $ go t False
  where
    go (Fork a b) c =
      case go a c of
        (a', True) -> (Fork a' b, True)
        (a', False) -> -- failed to find split in left subtree
          case go b False of
            (b',c) -> (Fork a b', c)
    go (Leaf n) False  | n >= 10 = (Fork (Leaf a) (Leaf (n - a)), True)
                       | otherwise = (Leaf n, False)
      where
        a = n `div` 2
    go (Leaf n) True = undefined


-- doSplit t = fst $ go t id
--   where
--     go (Fork a b) k = go a (\(a',c) -> if c then k (Fork a' b, c) else go b (\(b',c) -> k (Fork a' b', c)))
--     go (Leaf n) k  | n >= 10 = k (Fork (Leaf a) (Leaf (n - a)), True)
--                    | otherwise = k (Leaf n, False)
--       where
--         a = n `div` 2

    -- 
-- firstRegLeft l@(Leaf _, Top) = Nothing
-- firstRegLeft l@(Leaf _, _) = Just l
-- firstRegLeft l = firstRegLeft (left (up l))

-- ind r@(Leaf n,_) [] = r
-- ind t (True:xs) = ind (left t) xs
-- ind t (False:xs) = ind (right t) xs
-- firstLeft l = go l id
--   where
--     go l k = case mleft l of
--                Just r@(Leaf _, _) -> k r
--                Just l' -> go (mleft l') k
--                Nothing -> undefined

-- showBin :: Int -> String
-- showBin x = showIntAtBase 2 intToDigit x ""

-- hexToBin :: String -> String
-- hexToBin = concatMap (pad4 . showBin . digitToInt)

-- pad4 :: String -> String
-- pad4 l = replicate (4 - length l) '0' ++ l

-- oz :: Parser Char
-- oz = oneOf "01"
tok :: Parser a -> Parser a
tok p = p <* space


eol = char '\n'

-- |Parse a symbol, consuming leading whitespace.
symb :: String -> Parser String
symb = tok . string

-- |Parse a number.
nat :: Parser Int
nat = read <$> many1 digit

-- |Like 'nat' but consumes leading whitespace.
natural = tok nat

-- |Parse a negative number.
negnat = string "-" >> (-) 0 <$> natural

-- |Parse a Scheme number.
schemeNum = negnat <||> natural



main = do
  let dayNumber = 18  :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  Right inp <- traverse (parse pp "") . lines <$> readFile dayFilename
  let pres = (recur'' show (\x y -> "[" ++ x ++ "," ++ y ++ "]"))
  mapM_ print (pres <$> (scanl1 addS (tToTree <$> inp)))
  -- print (part1 inp)
  -- print (part2 inp)
  -- defaultMain
  --   [ bgroup
  --       dayString
  --       [ bench "part1" $ whnf part1 inp,
  --         bench "part2" $ whnf part2 inp
  --       ]
  --   ]
