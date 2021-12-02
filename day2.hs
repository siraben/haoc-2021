import Control.Applicative
import Control.Monad
import Criterion.Main
import Data.Functor
import Data.List
import qualified Text.ParserCombinators.ReadP as P

data Dir = F Int | D Int | U Int deriving (Show, Eq)

-- Horizontal, depth
type Pos = (Int, Int)

move :: Pos -> Dir -> Pos
move (h, d) (F n) = (h + n, d)
move (h, d) (U n) = (h, d - n)
move (h, d) (D n) = (h, d + n)

type Pos2 = (Int, Int, Int)

{-
    down X increases your aim by X units.
    up X decreases your aim by X units.
    forward X does two things:
        It increases your horizontal position by X units.
        It increases your depth by your aim multiplied by X.
-}
move2 :: Pos2 -> Dir -> Pos2
move2 (h, d, a) (F n) = (h + n, d + a * n, a)
move2 (h, d, a) (U n) = (h, d, a - n)
move2 (h, d, a) (D n) = (h, d, a + n)

pp = (f <|> d <|> u) <*> (P.skipSpaces *> P.readS_to_P reads) <* P.char '\n'
  where
    f = P.string "forward" $> F
    u = P.string "up" $> U
    d = P.string "down" $> D

part1 inp' = uncurry (*) (foldl' move (0, 0) inp')

part2 inp' = let (a, b, c) = foldl' move2 (0, 0, 0) inp' in a * b

main = do
  let dayNumber = 2 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let (inp', _) = last (P.readP_to_S (P.many pp) inp)
  print (part1 inp')
  print (part2 inp')
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 inp',
          bench "part2" $ whnf part2 inp'
        ]
    ]
