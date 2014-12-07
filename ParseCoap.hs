module Main where

import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as B

data CoAPMessage = CoAPMessage {
  version     :: Int,
  messageType :: CoAPMessageType,
  code        :: Int,
  messageID   :: Int,
  token       :: Int
} deriving (Show)

data CoAPMessageType = CON | NON | ACK | RST
  deriving (Show)

makeCoAP :: Int -> CoAPMessageType -> Int -> Int -> Int -> CoAPMessage
makeCoAP v t c mid tok =
  CoAPMessage { version = v, messageType = t, code = c, messageID = mid, token = tok }

typeFromFirstByte :: Word8 -> CoAPMessageType
typeFromFirstByte b =
  case t of 0 -> CON
            1 -> NON
            2 -> ACK
            3 -> RST
  where t = shiftR b 4 .&. 3

parseCoAP :: B.ByteString -> CoAPMessage
parseCoAP msg = makeCoAP 1 (typeFromFirstByte . head $ B.unpack msg) 0 0 0

main :: IO ()
main = B.getContents >>= \contents -> putStr . show $ parseCoAP contents
