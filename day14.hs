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
freqs :: [Int] -> IntMap Int
freqs = IM.fromListWith (+) . map (,1) . toList

type Rule = IntMap (IntMap Int)

type Str = IntMap Int

type Rules = IntMap Char

to :: (Char, Char) -> Int
to (a, b) = ord a * 100 + ord b

from :: Int -> (Char, Char)
from n = (chr (n `div` 100), chr (n `mod` 100))

pp :: Parser (String, Rules)
pp = do
  a <- manyTill upper eol
  eol
  b <- sepEndBy1 ((,) <$> (curry to <$> upper <*> upper) <*> (string " -> " *> upper)) eol
  pure (a, IM.fromList b)

-- | Apply a function @n@ times
apN :: Int -> (a -> a) -> a -> a
apN 0 f !x = x
apN !n f !x = apN (n - 1) f (f x)

rulesToMatrix :: Rules -> Rule
rulesToMatrix = IM.mapWithKey (\n c' -> let c = ord c' in IM.fromList (map (,1) [100 * (n `div` 100) + c, c * 100 + (n `mod` 100)]))

convString :: String -> Str
convString = freqs . map to . (zip `ap` tail)

apRule :: Rule -> Str -> Str
apRule m = unS . IM.foldMapWithKey (\k b -> S (IM.map (* b) (m IM.! k)))

newtype S = S (IntMap Int)

unS :: S -> IntMap Int
unS (S m) = m

instance Monoid S where
  mempty = S IM.empty
instance Semigroup S where
  S a <> S b = S $ IM.unionWith (+) a b

finalize :: Char -> Str -> Str
finalize c s = unS (S (IM.singleton (ord c) 1) <>  IM.foldMapWithKey (\a n -> S (IM.singleton (a `div` 100) n)) s)

part1 :: (Char, Rule, Str) -> Int
part1 = solve 10

part2 :: (Char, Rule, Str) -> Int
part2 = solve 40

solve :: Int -> (Char, Rule, Str) -> Int
solve n (c, r', s') = f (finalize c (apN n (apRule r') s'))
  where
    f m = maximum m - minimum m

main :: IO ()
main = do
  let dayNumber = 14 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  Right (s, r) <- parseFromFile pp dayFilename
  let s' = convString s
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
