module Main where
--
-- hex file -> hex file internal
-- hex file internal -> memory internal
-- memory internal -> flash commands
--

import Data.Char as DC
import Data.Word as DW
import Data.Vector as DV
import Text.Trifecta as TT
import Text.Parser.LookAhead as TPL
import Control.Monad as CM
import Control.Applicative as CA
import System.Environment as SE

data HFLine =
  HFValues
  { hfLineAddrL :: Word16
  , hfLineData :: Vector Word32 } |
  HFUpper
  { hfLineAddrU :: Word16 } |
  HFEOF
  deriving (Show)


-- Fail with given error message if condition doesn't hold
--
-- True  "oops" -> return ()
-- False "oops" -> mzero <?> "oops"
hfCheck :: MonadPlus m => Parsing m => Bool -> String -> m ()
hfCheck True  _ = return ()
hfCheck False e = mzero <?> e

-- Parse a Word8 expressed in hexadecimal and return it
--
-- "10" -> 0x10
-- "2f" -> 0x2f
hfHexWord8 :: CharParsing m => m Word8
hfHexWord8 = (\d1 d0 -> d1*16+d0) <$> digit <*> digit <?> "hex word8"
  where
    digit = (fromIntegral . digitToInt) <$> hexDigit

-- Parse a Word16 expressed in little-endian hexadecimal and return it
--
-- "1023" -> 0x2310
-- "2fd1" -> 0xd12f
hfHexWord16LE :: CharParsing m => m Word16
hfHexWord16LE = (\d0 d1 -> d1*256+d0) <$> digit <*> digit <?> "hex little-endian word16"
  where
    digit = fromIntegral <$> hfHexWord8

-- Parse a Word16 expressed in big-endian hexadecimal and return it
--
-- "1023" -> 0x1023
-- "2fd1" -> 0x2fd1
hfHexWord16BE :: CharParsing m => m Word16
hfHexWord16BE = (\d1 d0 -> d1*256+d0) <$> digit <*> digit <?> "hex big-endian word16"
  where
    digit = fromIntegral <$> hfHexWord8

-- Parse a Word32 expressed in little-endian hexadecimal and return it
--
-- "102368af" -> 0xaf682310
-- "2fd12107" -> 0x0721d12f
hfHexWord32LE :: CharParsing m => m Word32
hfHexWord32LE = (\d0 d1 -> d1*65536+d0) <$> digit <*> digit <?> "hex little-endian word32"
  where
    digit = fromIntegral <$> hfHexWord16LE

-- Verify checksum (sum `mod` 256 == 0) of a hex dump line without consuming it (no leading ':')
--
-- "020000040000fa" -> True
-- "020000040000fb" -> False
hfLineCSum :: LookAheadParsing m => CharParsing m => m Bool
hfLineCSum = (== 0) <$> lookAhead (Prelude.foldr (+) 0 <$> some hfHexWord8) <?> "line checksum"

-- Parse any end-of-line
--
-- CR, CR LF, LF, LF CR, or EOF
hfEOL :: MonadPlus m => CharParsing m => m ()
hfEOL = do
  ( char '\n' >> (optional $ char '\r') >> return ()) <|>
    ( char '\r' >> (optional $ char '\n') >> return ()) <|>
      eof <?> "newline"
  return ()

-- Parse a hex dump line
--
-- ":10598c009e007800040078000bef0700446b800049"
--                   -> HFValues $ fromList [0x9e,0x00,0x78,0x00,0x04,0x00,0x78,0x00,
--                                           0x0b,0xef,0x07,0x00,0x44,0x6b,0x80,0x00]
-- ":00000001FF"     -> HFEOF
-- ":0200000401f009" -> HFUpper 0x01f0
hfLine :: MonadPlus m => LookAheadParsing m => CharParsing m => m HFLine
hfLine = do
  char ':' <?> "line should have leading ':'"

  okay           <- hfLineCSum
  hfCheck okay "hex bytes should sum to 0 mod 256"

  values_length  <- fromIntegral <$> hfHexWord8
  address_lower  <- fromIntegral <$> hfHexWord16BE
  command        <- fromIntegral <$> hfHexWord8

  hfline         <-
    case command of
      0 -> do
        hfCheck (values_length `rem` 4 == 0) "length should be multiple of 4 for 32 bit LE data"
        values <- fromList <$> count (values_length `quot` 4) hfHexWord32LE
        return $ HFValues (fromIntegral address_lower) values

      1 -> do
        hfCheck (values_length == 0) "EOF type should have data length 0"
        return HFEOF

      4 -> do
        hfCheck (address_lower == 0) "upper address type should have zero address"
        hfCheck (values_length == 2) "upper address type should have data length 2"
        address_upper <- hfHexWord16BE
        return $ HFUpper (fromIntegral address_upper)

      _ -> mzero <?> ("unsupported type code " Prelude.++ show command)

  hfHexWord8

  hfEOL <?> "line should end after checksum"

  return hfline


main :: IO ()
main = do
  [file] <- getArgs
  result <- parseFromFile (many hfLine) file
  print result

