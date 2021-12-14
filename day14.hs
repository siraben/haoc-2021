{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Criterion.Main
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

eol :: Parser Char
eol = newline

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

type Rules = Map (Char, Char) Char

pp :: Parser (String, Rules)
pp = do
  a <- manyTill upper eol
  eol
  b <- sepEndBy1 ((,) <$> ((,) <$> upper <*> upper) <*> (string " -> " *> upper)) eol
  pure (a, M.fromList b)

-- | Apply a function @n@ times
apN :: Int -> (a -> a) -> a -> a
apN 0 f !x = x
apN !n f !x = apN (n - 1) f (f x)

type CPair = (Char, Char)

type Rule = Map CPair (Map CPair Int)

type Str = Map CPair Int

rulesToMatrix :: Map CPair Char -> Rule
rulesToMatrix = M.mapWithKey (\(a, b) c -> foo (a, b, c))
  where
    foo (a, b, c) = M.fromList (map (,1) [(a, c), (c, b)])

stringToVec :: String -> Map (Char, Char) Int
stringToVec = freqs . (zip `ap` tail)

apRule m = M.mapKeysWith (+) (m M.!)

distrb :: Map CPair Int -> Int -> Map CPair Int
distrb m i = M.map (* i) m

norm :: Map (Map CPair Int) Int -> Str
norm = M.foldlWithKey' (\m k c -> M.unionWith (+) m (distrb k c)) M.empty

thing :: Rule -> Str -> Str
thing r s = norm (apRule r s)

finalize :: Char -> Map CPair Int -> Map Char Int
finalize c = M.foldlWithKey' (\m (a, _) n -> M.insertWith (+) a n m) (M.singleton c 1)

part1 :: (Char, Rule, Str) -> Int
part1 = solve 10

part2 :: (Char, Rule, Str) -> Int
part2 = solve 40

solve :: Int -> (Char, Rule, Str) -> Int
solve n (c, r', s') = f (finalize c (apN n (thing r') s'))
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
