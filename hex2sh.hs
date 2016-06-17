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
  , hfLineData :: Vector Word8 } |
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

-- Parse n hex letters and return accumulated sum ((... (h_{n-1})*16 ... + h_2)*16 + h_1)*16 + h_0
--
-- "10"     2 -> 0x10
-- "598c00" 6 -> 0x598c00
hfHexLetters :: CharParsing m => Int -> m Integer
hfHexLetters n = Prelude.foldl accumulator 0 <$> digits <?> (show n Prelude.++ " hex letters")
  where
    accumulator s d = s*16 + d
    digits = count n (toInteger . digitToInt <$> hexDigit)

-- Parse n hex bytes and return vector of them
--
-- "10"     1 -> fromList [0x10]
-- "598c00" 3 -> fromList [0x59,0x8c,0x00]
hfHexVector :: CharParsing m => Int -> m (Vector Word8)
hfHexVector n = fromList <$> count n (fromIntegral <$> hfHexLetters 2) <?> ((show n) Prelude.++ " two digit hex letters")

-- Verify checksum (sum `mod` 256 == 0) of a hex dump line without consuming it (no leading ':')
--
-- "020000040000fa" -> True
-- "020000040000fb" -> False
hfLineCSum :: LookAheadParsing m => CharParsing m => m Bool
hfLineCSum = (== 0) <$> lookAhead (Prelude.foldr (+) 0 <$> some word8) <?> "line checksum"
  where
    word8 :: CharParsing m => m Word8
    word8 = fromIntegral <$> hfHexLetters 2

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

  values_length  <- hfHexLetters 2
  address_lower  <- hfHexLetters 4
  command        <- hfHexLetters 2

  hfline         <-
    case command of
      0 -> do
        values <- hfHexVector (fromIntegral values_length)
        return $ HFValues (fromIntegral address_lower) values

      1 -> do
        hfCheck (values_length == 0) "EOF type should have data length 0"
        return HFEOF

      4 -> do
        hfCheck (address_lower == 0) "upper address type should have zero address"
        hfCheck (values_length == 2) "upper address type should have data length 2"
        address_upper <- hfHexLetters 4
        return $ HFUpper (fromIntegral address_upper)

      _ -> mzero <?> ("unsupported type code " Prelude.++ show command)

  hfHexLetters 2

  hfEOL <?> "line should end after checksum"

  return hfline


main :: IO ()
main = do
  [file] <- getArgs
  result <- parseFromFile (many hfLine) file
  print result

