module Main where

import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as B

data CoAPMessage = CoAPMessage {
  version     :: Word8,
  messageType :: CoAPMessageType,
  code        :: Word8,
  messageID   :: Int,
  token       :: Int
} deriving (Show)

data CoAPMessageType = CON | NON | ACK | RST
  deriving (Show)

makeCoAP :: Word8 -> CoAPMessageType -> Word8 -> Int -> Int -> CoAPMessage
makeCoAP v t c mid tok =
  CoAPMessage { version = v, messageType = t, code = c, messageID = mid, token = tok }

parseCoAP :: B.ByteString -> CoAPMessage
parseCoAP bs = makeCoAP (coapVersion bs) (coapMessageType bs) (coapCode bs) 0 0

coapVersion :: B.ByteString -> Word8
coapVersion bs = shiftR (B.head bs) 6

coapMessageType :: B.ByteString -> CoAPMessageType
coapMessageType bs =
  case t of 0 -> CON
            1 -> NON
            2 -> ACK
            3 -> RST
  where t = shiftR (B.head bs) 4 .&. 3

coapCode :: B.ByteString -> Word8
coapCode = B.head . B.tail

main :: IO ()
main = B.getContents >>= \bs -> putStr . show $ parseCoAP bs
