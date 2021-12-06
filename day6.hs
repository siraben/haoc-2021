{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import Criterion.Main
import qualified Data.Text as T

type V = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

final :: V -> Int
final = sum . back

back :: V -> [Int]
back (a, b, c, d, e, f, g, h, i) = [a, b, c, d, e, f, g, h, i]

part1 :: V -> Int
part1 = final . apN 80 step

part2 :: V -> Int
part2 = final . apN 256 step

freqs' :: [Int] -> V
freqs' l = f $ map (\x -> length $ filter (== x) l) [1 .. 5]
  where
    f [a, b, c, d, e] = (0, a, b, c, d, e, 0, 0, 0)
    f _ = undefined

step :: V -> V
step (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h + a, i, a)

-- | Apply a function @n@ times
apN :: Int -> (a -> a) -> a -> a
apN 0 f !x = x
apN !n f !x = apN (n - 1) f (f x)

main = do
  let dayNumber = 6 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map read . splitOn "," <$> readFile dayFilename
  let inp' = freqs' inp
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
