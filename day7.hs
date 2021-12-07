{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Data.Foldable
import Data.Function
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

minMax l = [(minimum l) .. (maximum l)]

part1 l = fst $ minimumBy (compare `on` fst) $ map f $ minMax l
  where
    alignAt n = sum $ map (\x -> abs (x - n)) l
    f x = (alignAt x, x)

sumTo n = (n * (n + 1)) `div` 2

part2 l = fst $ minimumBy (compare `on` fst) $ map f $ minMax l
  where
    alignAt n = sum $ map (\x -> sumTo (abs (x - n))) l
    f x = (alignAt x, x)

main = do
  let dayNumber = 7 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- map (read @Int) . splitOn "," <$> readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
