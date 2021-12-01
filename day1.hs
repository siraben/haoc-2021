import Criterion.Main
import Control.Monad
import Data.List

slidingWindows :: Int -> [Int] -> [[Int]]
slidingWindows n l = take n <$> tails l

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

countIncrs = countIf (== LT) . (zipWith compare `ap` tail)

part1 = countIncrs

part2 l = countIncrs (sum <$> slidingWindows 3 l)

main :: IO ()
main = do
  let dayNumber = 1
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = read <$> inp :: [Int]
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
