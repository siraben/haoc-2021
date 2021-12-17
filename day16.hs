import Criterion.Main
import Data.Char
import Data.Foldable
import Numeric
import Text.ParserCombinators.Parsec

(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

showBin :: Int -> String
showBin x = showIntAtBase 2 intToDigit x ""

hexToBin :: String -> String
hexToBin = concatMap (pad4 . showBin . digitToInt)

pad4 :: String -> String
pad4 l = replicate (4 - length l) '0' ++ l

oz :: Parser Char
oz = oneOf "01"

fromBinNum :: String -> Int
fromBinNum = foldl' f 0
  where
    f n '1' = n * 2 + 1
    f n '0' = n * 2
    f n _ = undefined

data Packet = Packet
  { version :: Int,
    typeId :: Int,
    payload :: Either [Packet] Int
  }
  deriving (Show, Eq)

part1 :: Packet -> Int
part1 Packet {version = v, payload = Right _} = v
part1 Packet {version = v, payload = Left l} = v + sum (map part1 l)

part2 :: Packet -> Int
part2 Packet {typeId = n, payload = l} = either (f n . map part2) id l
  where
    f 0 = sum
    f 1 = product
    f 2 = minimum
    f 3 = maximum
    f 4 = head
    f 5 = \[a, b] -> fromEnum $ a > b
    f 6 = \[a, b] -> fromEnum $ a < b
    f 7 = \[a, b] -> fromEnum $ a == b

emptyPacket :: Packet
emptyPacket = Packet 0 0 (Right 0)

litPayload :: Parser Int
litPayload = fromBinNum . concat <$> litPayload'

litPayload' :: Parser [String]
litPayload' = do
  b <- oz
  if b == '1' then (:) <$> count 4 oz <*> litPayload' else pure <$> count 4 oz

parsePacket :: Parser Packet
parsePacket = do
  packetVersion <- fromBinNum <$> count 3 oz
  packetTypeId <- fromBinNum <$> count 3 oz
  payload <- case packetTypeId of
    4 -> Right <$> litPayload
    _ -> Left <$> operator
  pure (Packet packetVersion packetTypeId payload)
  where
    operator = do
      b <- oz
      n <-
        fromBinNum <$> case b of
          '0' -> count 15 oz -- total length in bits of subpackets
          '1' -> count 11 oz -- number of subpackets contained
          _ -> fail "impossible"
      case b of
        '1' -> count n parsePacket
        '0' -> do
          b <- count n oz
          case parse (many1 parsePacket) "" b of
            Right x -> pure x
            _ -> fail "failed to parse subpacket"
        _ -> fail "impossible"

pp = parse parsePacket "" . hexToBin

main = do
  let dayNumber = 16 :: Int
  let dayString = "day" <> show dayNumber
  let dayFilename = dayString <> ".txt"
  inp <- readFile dayFilename
  let Right p = pp inp
  print (part1 p)
  print (part2 p)
  defaultMain
    [ bgroup
        dayString
        [ bench "part1" $ whnf part1 p,
          bench "part2" $ whnf part2 p
        ]
    ]
