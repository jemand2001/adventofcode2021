module Main where

-- import Debug.Trace
import Data.Maybe
import Utils

trace :: String -> b -> b
trace = tsnoc

data Payload = VPayload Integer | PPayload [Packet] deriving (Show, Eq)

data PacketType = Sum | Product | Minimum | Maximum | Greater | Less | Equal deriving (Show, Eq)

instance Enum PacketType where
  toEnum 0 = Sum
  toEnum 1 = Product
  toEnum 2 = Minimum
  toEnum 3 = Maximum
  toEnum 5 = Greater
  toEnum 6 = Less
  toEnum 7 = Equal
  toEnum _ = undefined
  fromEnum Sum = 0
  fromEnum Product = 1
  fromEnum Minimum = 2
  fromEnum Maximum = 3
  fromEnum Greater = 5
  fromEnum Less = 6
  fromEnum Equal = 7

data Packet
  = ValuePacket
      { pVersion :: Int,
        pPayload :: Payload
      }
  | OperatorPacket
      { pVersion :: Int,
        pType :: PacketType,
        pPayload :: Payload
      }
  deriving (Show, Eq)

hexDigitToString :: Char -> String
hexDigitToString '0' = "0000"
hexDigitToString '1' = "0001"
hexDigitToString '2' = "0010"
hexDigitToString '3' = "0011"
hexDigitToString '4' = "0100"
hexDigitToString '5' = "0101"
hexDigitToString '6' = "0110"
hexDigitToString '7' = "0111"
hexDigitToString '8' = "1000"
hexDigitToString '9' = "1001"
hexDigitToString 'A' = "1010"
hexDigitToString 'B' = "1011"
hexDigitToString 'C' = "1100"
hexDigitToString 'D' = "1101"
hexDigitToString 'E' = "1110"
hexDigitToString 'F' = "1111"
hexDigitToString _ = undefined

hexToString :: String -> String
hexToString = concatMap hexDigitToString

parseValueGroups :: String -> (Integer, String)
parseValueGroups ('0' : s) = (fromIntegral $ binToInt $ take 4 s, drop 4 s)
parseValueGroups ('1' : s) = (((2 ^ 4) * read (take 4 s)) + other, rest)
  where
    (other, rest) = parseValueGroups $ drop 4 s
parseValueGroups _ = error "non-binary string in parseValueGroups"

addPacket :: ([Packet], String) -> ([Packet], String)
addPacket (ps, s)
  | null s = (ps, s)
  | all (== '0') s = trace "reached tail of string" (ps, "")
  | otherwise = trace ("adding packet " ++ show p) (p : ps, s')
  where
    (p, s') = parseBinary s

addPacket' :: String -> ([Packet], [Char]) -> ([Packet], [Char])
addPacket' toParse (ps, s') = addPacket (ps, if null s' && null ps then toParse else s')

parseContained :: String -> ([Packet], String)
parseContained ('0' : s) = trace "length id 0" (reverse packets, rest)
  where
    total = binToInt $ take 15 s
    toParse = take total $ drop 15 s
    rest = drop (total + 15) s
    (packets, "") = foldr (const addPacket) ([], toParse) [0 .. total]
parseContained ('1' : s) = trace "length id 1" (reverse packets, rest)
  where
    count = binToInt $ take 11 s
    toParse = drop 11 s
    (packets, rest) = foldr (const $ addPacket' toParse) ([], "") [1 .. count]
parseContained s
  | all (== '0') s = ([], "")
  | otherwise = error s

parseBinary :: String -> (Packet, String)
parseBinary s
  | all (`elem` ['0', '1']) s = parseBinary'
  | otherwise = error "you gave a length too long, or a non-binary string"
  where
    parseBinary'
      | type_ == 4 = trace ("value packet: " ++ s) (ValuePacket version $ VPayload values, vRest)
      | otherwise = trace ("operator packet (type = " ++ show type_ ++ ") : " ++ s) (OperatorPacket version (toEnum type_) $ PPayload packets, pRest)
    version = binToInt $ take 3 s
    type_ = binToInt $ take 3 $ drop 3 s
    (values, vRest) = parseValueGroups $ drop 6 s
    (packets, pRest) = parseContained $ drop 6 s

versionSum :: Packet -> Int
versionSum ValuePacket {pVersion = v} = v
versionSum OperatorPacket {pVersion = v, pPayload = (PPayload ps)} = v + sum (map versionSum ps)
versionSum _ = undefined

eval :: Packet -> Integer
eval ValuePacket {pPayload = VPayload x} = x
eval OperatorPacket {pType = Sum, pPayload = PPayload xs} = sum $ map eval xs
eval OperatorPacket {pType = Product, pPayload = PPayload xs} = product $ map eval xs
eval OperatorPacket {pType = Minimum, pPayload = PPayload xs} = minimum $ map eval xs
eval OperatorPacket {pType = Maximum, pPayload = PPayload xs} = maximum $ map eval xs
eval OperatorPacket {pType = Greater, pPayload = PPayload [p1, p2]} = fromIntegral $ fromEnum $ eval p1 > eval p2
eval OperatorPacket {pType = Less, pPayload = PPayload [p1, p2]} = fromIntegral $ fromEnum $ eval p1 < eval p2
eval OperatorPacket {pType = Equal, pPayload = PPayload [p1, p2]} = fromIntegral $ fromEnum $ eval p1 == eval p2
eval p = error $ show p

part1 :: String -> String
part1 = evaluate (fst . parseBinary . hexToString . filter (/= '\n')) versionSum

part2 :: String -> String
part2 = evaluate (fst . parseBinary . hexToString . filter (/= '\n')) eval

tests :: [String]
tests = [
    "C200B40A82"
  , "04005AC33890"
  , "880086C3E88112"
  , "CE00C43D881120"
  , "D8005AC2A8F0"
  , "F600BC2D8F"
  , "9C005AC2F8F0"
  , "9C0141080250320F1802104A08"
  ]


test = map (eval . fst . parseBinary . hexToString) tests

main :: IO ()
-- main = print test
main = interact part2
