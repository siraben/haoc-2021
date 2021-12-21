import Criterion.Main
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- Slow splitOn for prototyping
splitOn :: String -> String -> [String]
splitOn sep s = T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

smol (x : _) = isLower x
smol _ = undefined

-- from cave a and small-cave-visited-set s, return a new list of
-- (cave,set) pairs
expand g (a, s) = maybe [] f (g M.!? a)
  where
    -- return new caves we can visit (except already-visited small ones)
    -- and the updated list of small caves we have visited
    f x = [(c, if smol c then S.insert c s else s) | c <- x, c `S.notMember` s]

expand2 g (a, s1, s2) = maybe [] f (g M.!? a)
  where
    -- return new caves we can visit (except already-visited small ones)
    -- and the updated list of small caves we have visited
    -- c hasn't been visited before or it has been and it's not in the secondary visited set yet
    f x = concat prep
      where
        prep =
          [ if smol c && c `S.member` s1 && s2 -- if c is a small cave and we can still disallow re-entry
              then [(c, S.insert c s1, False)] -- mark it is visited and do not allow duplicate entries
              -- otherwise c is not a small cave or we haven't visited it yet or we disallow re-entry
              -- check if c is a small cave we have visited before, and that we can still re-enter
              else
                if smol c && c `S.member` s1 && c /= "start" && c /= "end" && s2
                  then [(c, s1, False) | not (smol c)] -- if so then visit and disallow re-entry
                  -- otherwise x is not a small or unvisited cave and we disallow re-entry
                  else
                    if not (smol c) -- if x is not small, just visit it
                      then [(c, s1, s2)]
                      else -- x must be small and unvisited, so visit it
                        [(c, S.insert c s1, s2) | c `S.notMember` s1]
            | c <- x
          ]

solve :: Map String [String] -> String -> String -> Int
solve g a b = go [(a, S.singleton a)] 0
  where
    go [] n = n
    go l n = go xs (n + n')
      where
        -- l' is the paths in consideration that end at b
        (l', xs) = partition ((== b) . fst) (concatMap (expand g) l)
        n' = length l'

solve2 :: Map String [String] -> String -> String -> Int
solve2 g a b = go [(a, S.singleton a, True)] 0
  where
    go [] n = n
    go l n = go xs (n + n')
      where
        -- l' is the paths in consideration that end at b
        (l', xs) = partition (\(x, y, z) -> x == b) (concatMap (expand2 g) l)
        n' = length l'

makeUndirected = M.map (delete "start") . M.insert "end" []

part1 g = solve g "start" "end"

part2 g = solve2 g "start" "end"

main = do
  let dayNumber = 12 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- makeUndirected . M.fromListWith (++) . concatMap ((\[x, y] -> [(x, [y]), (y, [x])]) . splitOn "-") . lines <$> readFile dayFilename
  print (part1 inp)
  print (part2 inp)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp,
          bench "part2" $ whnf part2 inp
        ]
    ]
