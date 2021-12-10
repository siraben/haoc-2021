import Criterion.Main
import Data.List

badness :: [Char] -> (Int, [Char])
badness s = go s []
  where
    go ('(' : r) s = go r (')' : s)
    go ('{' : r) s = go r ('}' : s)
    go ('[' : r) s = go r (']' : s)
    go ('<' : r) s = go r ('>' : s)
    go (a : r) (b : s) | a == b = go r s
    go (')' : r) a = (3, a)
    go (']' : r) a = (57, a)
    go ('}' : r) a = (1197, a)
    go ('>' : r) a = (25137, a)
    go [] s = (0, s)
    go _ _ = undefined

corrupted :: String -> Bool
corrupted = (> 0) . fst . badness

median :: [Int] -> Int
median [] = undefined
median zs = go zs zs
  where
    go (x0 : _) [_] = x0
    go (x0 : x1 : _) [_, _] = (x0 + x1) `div` 2
    go (_ : xs) (_ : _ : ys) = go xs ys
    go _ _ = undefined

part1 = sum . map (fst . badness)

part2 :: [(Int, String)] -> Int
part2 = median . sort . map (foldl' f 0 . snd) . filter ((== 0) . fst)
  where
    f acc ')' = acc * 5 + 1
    f acc ']' = acc * 5 + 2
    f acc '}' = acc * 5 + 3
    f acc '>' = acc * 5 + 4
    f _ _ = undefined

main = do
  let dayNumber = 10 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- lines <$> readFile dayFilename
  let inp' = map badness inp
  print (part1 inp)
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp'
        ]
    ]
