{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Criterion.Main
import Data.Char
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

eol :: Parser Char
eol = newline

-- | Build a frequency map
-- freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = IM.fromListWith (+) . map (,1) . toList

type Rule = IntMap (IntMap Int)

type Str = IntMap Int

type Rules = IntMap Char

to (a, b) = ord a * 100 + ord b

from n = (chr (n `div` 100), chr (n `mod` 100))

pp :: Parser (String, Rules)
pp = do
  a <- manyTill upper eol
  eol
  b <- sepEndBy1 ((,) <$> ((curry to <$> upper <*> upper)) <*> (string " -> " *> upper)) eol
  pure (a, IM.fromList b)

-- | Apply a function @n@ times
apN :: Int -> (a -> a) -> a -> a
apN 0 f !x = x
apN !n f !x = apN (n - 1) f (f x)

--rulesToMatrix :: IntMap Char -> Rule
rulesToMatrix = IM.mapWithKey (\n c -> let (a, b) = from n in foo (a, b, c))
  where
    foo (a, b, c) = IM.fromList (map (,1) (to <$> [(a, c), (c, b)]))

stringToVec :: String -> IntMap Int
stringToVec = freqs . map to . (zip `ap` tail)

-- apRule :: Num a => IntMap IM.Key -> IntMap a -> IntMap a
apRule m = M.mapKeysWith (+) (m IM.!)

distrb :: IntMap Int -> Int -> IntMap Int
distrb m i = IM.map (* i) m

-- norm :: Map (IntMap Int) Int -> Str
norm = M.foldlWithKey' (\m k c -> IM.unionWith (+) m (distrb k c)) IM.empty

-- thing :: Rule -> Str -> Str
thing r s = norm (apRule r s)

-- finalize :: Char -> IntMap Int -> IntMap Int
finalize c = IM.foldlWithKey' (\m a n -> IM.insertWith (+) (a `div` 100) n m) (IM.singleton (ord c) 1)

part1 :: (Char, Rule, Str) -> Int
part1 = solve 10

part2 :: (Char, Rule, Str) -> Int
part2 = solve 40

-- solve :: Int -> (Char, Rule, Str) -> Int
solve n (c, r', s') = f (finalize c (apN n (thing r' . M.fromList . IM.toList) s'))
  where
    f m = maximum m - minimum m

main :: IO ()
main = do
  let dayNumber = 14 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  Right (s, r) <- parseFromFile pp dayFilename
  let s' = stringToVec s
  let r' = rulesToMatrix r
  let c = last s
  let inp = (c, r', s')
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
