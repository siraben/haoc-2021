import Criterion.Main

step :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
step (x, y, dx, dy) = (x + dx, y + dy, max 0 (dx -1), dy -1)

initialize :: (Int, Int) -> (Int, Int, Int, Int)
initialize (dx, dy) = (0, 0, dx, dy)

inTarget :: (Ord a1, Ord a2) => (a1, a1, a2, a2) -> (a1, a2, c, d) -> Bool
inTarget (lx, hx, ly, hy) (x, y, _, _) = lx <= x && x <= hx && ly <= y && y <= hy

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x : xs)
  | p x = x : takeWhile1 p xs
  | otherwise = [x]

sim :: (a, Int, Int, d) -> (Int, Int) -> [(Int, Int, Int, Int)]
sim (lx, hx, ly, hy) = takeWhile1 (\(x, y, _, _) -> (x < hx) && (y > ly)) . iterate step . initialize

hits :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
hits x y = any (inTarget x) (sim x y)

part1 :: (Int, Int, Int, Int) -> Int
part1 (lx, hx, ly, hy) = ((- ly) * ((- ly) - 1)) `div` 2

part2 :: (Int, Int, Int, Int) -> Int
part2 p@(lx, hx, ly, _) = length [(x, y) | x <- [10 .. hx], y <- [ly .. (- ly)], hits p (x, y)]

main = do
  let dayNumber = 17 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  let inp = (185, 221, -122, -74)
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
