{-# LANGUAGE NoMonomorphismRestriction #-}

import Criterion.Main
import Data.Foldable
import Data.List
import Data.Maybe

-- | Count the number of items in a container where the predicate is true.
countIf :: Foldable f => (a -> Bool) -> f a -> Int
countIf p = length . filter p . toList

f x = length x `elem` [2, 3, 4, 7]

lup = (fromJust .) . lookup

applyConstraint x ass = map (`lup` ass) x

eqSorted a b = sort a == sort b

mkCan can = zip can ['a' .. 'g']

apCan can = map (`applyConstraint` mkCan can)

-- is candidate a sol wrt l
isSol can l = eqSorted (map sort (apCan can l)) constraints

bbb = zip constraints [0 .. 9]

constraints :: [String]
constraints = [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9]
  where
    _0 = "abcefg"
    _1 = "cf"
    _2 = "acdeg"
    _3 = "acdfg"
    _4 = "bcdf"
    _5 = "abdfg"
    _6 = "abdefg"
    _7 = "acf"
    _8 = "abcdefg"
    _9 = "abcdfg"

combs = permutations ['a' .. 'g']

x :: ([String], [String])
x = (["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"], [])

toNum = foldl (\x y -> 10 * x + y) 0

sol = head . (\x -> filter (`isSol` x) (permutations ['a' .. 'g']))

part1 inp = sum $ countIf f . snd <$> inp

part2 inp = sum $ map (\(x, y) -> toNum (map ((`lup` bbb) . sort) (apCan (sol x) y))) inp

main :: IO ()
main = do
  let dayNumber = 8 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map ((\(a, _ : b) -> (a, b)) . splitAt 10 . words) . lines <$> readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
