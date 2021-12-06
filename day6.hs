{-# LANGUAGE TupleSections #-}

import Criterion.Main
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

final = sum . back

back (a, b, c, d, e, f, g, h, i) = [a, b, c, d, e, f, g, h, i]

part1 = final . ((!! 80) . iterate step)

part2 = final . ((!! 256) . iterate step)

freqs' l = f $ map (\x -> length $ filter (== x) l) [1 .. 5]
  where
    f [a, b, c, d, e] = (0, a, b, c, d, e, 0, 0, 0)
    f _ = undefined

step (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h + a, i, a)

main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map read . splitOn "," <$> readFile dayFilename
  let inp' = freqs' inp
  let l = iterate step inp' !! 80
  let l' = iterate step inp' !! 256
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
