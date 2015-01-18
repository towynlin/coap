module Main where

import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as B

data CoAPMessage = CoAPMessage {
  version     :: Word8,
  messageType :: CoAPMessageType,
  code        :: Word8,
  messageID   :: Word16,
  token       :: Int
} deriving (Show)

data CoAPMessageType = CON | NON | ACK | RST
  deriving (Show)

parseCoAP :: B.ByteString -> CoAPMessage
parseCoAP bs = CoAPMessage (parseVersion bs) (parseMessageType bs) (parseCode bs) (parseMessageID bs) 0

parseVersion :: B.ByteString -> Word8
parseVersion bs = shiftR (B.head bs) 6

parseMessageType :: B.ByteString -> CoAPMessageType
parseMessageType bs =
  case t of 0 -> CON
            1 -> NON
            2 -> ACK
            3 -> RST
  where t = shiftR (B.head bs) 4 .&. 3

parseCode :: B.ByteString -> Word8
parseCode bs = B.index bs 1

parseMessageID :: B.ByteString -> Word16
parseMessageID bs = shiftL (index16 2) 8 + index16 3
  where index16 i = fromIntegral (B.index bs i) :: Word16

main :: IO ()
main = B.getContents >>= \bs -> putStr . show $ parseCoAP bs
