{-# LANGUAGE TupleSections #-}

import Data.Foldable
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- Start working down here
part1 (mea, g) = M.size (M.filter id (step mea (step mea g)))

part2 i = undefined

type Pt = (Int, Int)

type Grid = Map Pt Bool

nbs :: Pt -> [Pt]
nbs (x, y) = [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1]]

fromBinNum :: [Bool] -> Int
fromBinNum = foldl' f 0
  where
    f n True = n * 2 + 1
    f n False = n * 2

toGrid :: [String] -> Grid
toGrid = M.fromList . concat . zipWith (\x l -> zipWith (curry (\((x, a), y) -> ((x, y), a == '#'))) (map (x,) l) [1 ..]) [1 ..]

frontier :: Map (Int, Int) a -> S.Set (Int, Int)
frontier = S.unions . S.map (S.fromList . nbs) . M.keysSet

render n s g = unlines [[if Just True == (g M.!? (x, y)) then '#' else '.' | y <- [(negate s) .. (n + s)]] | x <- [(negate s) .. (n + s)]]

calc :: Grid -> IntSet -> Pt -> Bool
calc g ime p = IS.member l' ime
  where
    f x = Just True == (g M.!? x)
    l = nbs p
    l' = fromBinNum (map f l)

step :: IntSet -> Grid -> Grid
step ime g = M.fromSet (calc g ime) fs
  where
    fs = frontier g

main :: IO ()
main = do
  let dayNumber = 20 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  [mea', g'] <- splitOn "\n\n" <$> readFile dayFilename
  let mea = IS.fromList (elemIndices '#' mea')
  let ls = lines g'
  let n = length (head ls)
  let g = toGrid ls
  print (part1 (mea, g))
-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]
