{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

final = sum . map snd

part1 = final . ((!! 80) . iterate step')

part2 = final . ((!! 256) . iterate step')

step' :: [(Int, Int)] -> [(Int, Int)]
step' l = M.toAscList (M.fromListWith (+) (concatMap f l))
  where
    f (0, x) = [(6, x), (8, x)]
    f (n, x) = [(n -1, x)]

main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map read . splitOn "," <$> readFile dayFilename
  let inp' = M.toAscList $ freqs inp
  let l = iterate step' inp' !! 80
  let l' = iterate step' inp' !! 256
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
