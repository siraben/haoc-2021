import Data.List
import qualified Data.Set as S
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

fold "x" n = S.map f
  where
    f (x, y)
      | x > n = (n - (x - n), y)
      | otherwise = (x, y)
fold "y" n = S.map f
  where
    f (x, y)
      | y > n = (x, n - (y - n))
      | otherwise = (x, y)
fold _ _ = undefined

render g = unlines [[if (x, y) `S.member` g then '#' else '.' | x <- [0 .. 50]] | y <- [0 .. 5]]

part1 (grid, instrs) = foldl' (\g (c, n) -> fold c n g) grid instrs

part2 = render

main = do
  let dayNumber = 13 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  [pts', instrs'] <- splitOn "\n\n" <$> readFile dayFilename
  let pts = (\[x, y] -> (x, y)) . map read . splitOn "," <$> lines pts'
  let instrs = concat $ map ((\[x, n] -> (x, read n)) . splitOn "=") . drop 2 . words <$> lines instrs'
  let grid = S.fromList pts
  let fin = part1 (grid, instrs)
  print (S.size (uncurry fold (head instrs) grid))
  putStrLn (render fin)

-- print (part1 inp)
-- print (part2 inp)
-- defaultMain
--   [ bgroup
--       dayString
--       [ bench "part1" $ whnf part1 inp,
--         bench "part2" $ whnf part2 inp
--       ]
--   ]
