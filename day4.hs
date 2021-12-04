{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Criterion.Main
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

type Bingo = [[(Int, Bool)]]

mark :: Bingo -> Int -> Bingo
mark b n = map (map change) b
  where
    change e@(m, b)
      | m == n = (m, True)
      | otherwise = e

unmarked :: Bingo -> [Int]
unmarked = map fst . filter snd . concat

initBoard :: [[Int]] -> Bingo
initBoard = map (map (,False))

f ns (b, y) = unmarkedNums * finalNum
  where
    unmarkedNums = sum $ map fst $ filter (not . snd) $ concat finalBoard
    finalNum = last nums
    finalBoard = foldl' mark (initBoard b) nums
    nums = take y ns

part1 (ns, inp') = f ns $ minimumBy (compare `on` snd) . zip inp' $ g ns . initBoard <$> inp'

part2 (ns, inp') = f ns $ maximumBy (compare `on` snd) . zip inp' $ g ns . initBoard <$> inp'

solved b = check b || check (transpose b)
  where
    check = any (all snd)

g ns b =
  scanl' mark b ns
    & map solved
    & findIndex id
    & fromJust

main = do
  let dayNumber = 4 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- unlines . map (dropWhile (== ' ')) . lines <$> readFile dayFilename
  let (ns' : inp'') = splitOn "\n\n" inp
  let ns = map (read :: String -> Int) $ splitOn "," ns'
  let inp' = map (map (read :: String -> Int) . words) . lines <$> inp''
  print (part1 (ns, inp'))
  print (part2 (ns, inp'))
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 (ns, inp'),
          bench "part2" $ whnf part2 (ns, inp')
        ]
    ]
