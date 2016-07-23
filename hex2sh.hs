module Main where
--
-- hex file -> hex file internal
-- hex file internal -> memory internal
-- memory internal -> flash commands
--

import Data.Char as DC
import Data.Word as DW
import Data.List as DL
import Text.Trifecta as TT
import Text.Parser.LookAhead as TPL
import Control.Monad as CM
import Control.Applicative as CA
import System.Environment as SE

data HFLine =
  HFLine
  { hfLineAddrL :: Word16
  , hfLineData :: [Word32] } |
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
--                   -> HFLine 0x598c $ fromList [0x0078009e,0x00780004,0x0007ef0b,0x00806b44]
-- ":00000001FF"     -> HFEOF
-- ":0200000401f009" -> HFUpper 0x01f0
hfLine :: MonadPlus m => LookAheadParsing m => CharParsing m => m HFLine
hfLine = do
  char ':' <?> "line should have leading ':'"

  okay           <- hfLineCSum
  hfCheck okay "hex bytes should sum to 0 mod 256"

  values_length  <- hfHexWord8
  address_lower  <- hfHexWord16BE
  command        <- hfHexWord8

  hfline         <-
    case command of
      0 -> do
        hfCheck (values_length `rem` 4 == 0) "length should be multiple of 4 for 32 bit LE data"
        values <- count (fromIntegral values_length `quot` 4) hfHexWord32LE
        return $ HFLine (fromIntegral address_lower) values

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

-- Parse hex file to 32-bit address value pairs
--
-- ":0200000401f009\n" -- HFUpper 0x01f0
-- ":10598c009e007800040078000bef0700446b800049\n"
--                     -- HFLine 0x598c [0x0078009e,0x00780004,0x0007ef0b,0x00806b44]
-- ":00000001FF\n"     -- HFEOF
--   -> [(0x01f0598c,0x0078009e),(0x01f05990,0x00780004),(0x01f05994,0x0007ef0b),(0x01f05998,0x00806b44)]
--
hfParse :: MonadPlus m => LookAheadParsing m => CharParsing m => m [(Word32,Word32)]
hfParse =  concat <$> doit 0
    where
      doit address_upper = do
        line <- hfLine
        case line of
          HFLine  address_lower code -> (:) <$> pure (addressAdd address_upper address_lower code) <*> doit address_upper
          HFUpper address_upper'     -> doit address_upper'
          HFEOF                      -> eof *> pure []
      addressAdd address_upper address_lower instructions =
          zipWith (\index instruction -> (address + index*4,instruction)) [0..] instructions
              where
                address = fromIntegral address_upper * 0x10000 + fromIntegral address_lower

-- Sort [(address,value)] by address
--
-- [(0x01f0598c,0x00780004),(0x01f05990,0x0078009e),(0x01f0598c,0x0007ef0b)]
--   -> [(0x01f0598c,0x00780004),(0x01f0598c,0x0007ef0b),(0x01f05990,0x0078009e)]
--
sparsevaluesSort :: [(Word32,Word32)] -> [(Word32,Word32)]
sparsevaluesSort = sortOn fst

-- Overwrite early address with later ones in sorted [(address,value)]
--
-- [(0x01f0598c,0x00780004),(0x01f0598c,0x0007ef0b),(0x01f05990,0x0078009e)]
--   -> [(0x01f0598c,0x0007ef0b),(0x01f05990,0x0078009e)]
--
sparsevaluesOverwrite :: [(Word32,Word32)] -> [(Word32,Word32)]
sparsevaluesOverwrite = map last . groupBy (\(address0,_) (address1,_) -> address0 == address1)


-- Group [(address,value)] into chunks sizes according to flash row size
--
-- 16 -> [(0x01f0598c,0x0078009e),(0x01f05990,0x00780004),(0x01f05994,0x0007ef0b),(0x01f05998,0x00806b44)]
--   ->  [[(0x01f0598c,0x0078009e)],[(0x01f05990,0x00780004),(0x01f05994,0x0007ef0b),(0x01f05998,0x00806b44)]]
--
sparsevaluesGroupRows :: Word32 -> [(Word32,Word32)] -> [[(Word32,Word32)]]
sparsevaluesGroupRows chunk = groupBy (\(address0,_) (address1,_) -> address0 `quot` chunk == address1 `quot` chunk)

-- Convert [(address,value)] chunks into (address,[(offset,value])
--
-- 16 -> [(0x01f05990,0x00780004),(0x01f05994,0x0007ef0b),(0x01f05998,0x00806b44)]
--   -> (0x01f05990,[(0x0,0x00780004),(0x4,0x0007ef0b),(0x8,0x00806b44)])
addressoffsetvaluesFromSparseValues :: Word32 -> [(Word32,Word32)] -> (Word32,[(Word32,Word32)])
addressoffsetvaluesFromSparseValues chunk address_values = (address_base,offset_values)
    where
      address_base = fst (head address_values) `quot` chunk * chunk
      offset_values = map (\(address,values) -> (address `rem` chunk,values)) address_values

-- Convert (address,[(offset,value)]) chunk to (address,[value]) by inserting default value
--
-- 16 -> 0x00ffffff -> (0x01f05990,[(0x0,0x00780004),(0x4,0x0007ef0b),(0x8,0x00806b44)])
--   -> (0x01f05990,[0x00780004,0x0007ef0b,0x00806b44,0x00ffffff])
addressvaluesFromAddressOffsetValues :: Word32 -> Word32 -> (Word32,[(Word32,Word32)]) -> (Word32,[Word32])
addressvaluesFromAddressOffsetValues chunk value_default (address,offsetvalues)
    = (address,(map snd . sparsevaluesOverwrite . sparsevaluesSort) (offsetvalues_default ++ offsetvalues))
    where
      offsetvalues_default = zip [0,4..chunk-1] (repeat value_default)

-- Convert (address,[value]) to write command
--
-- (0x01f05990,[0x00 78 00 04,0x00 07 ef 0b,0x00 80 6b 44,0x00 ff ff ff])
--   -> [0xf8,0x2c,0xc8, 0x01, 0x0d, 0x78,0x04,0x00, 0x07,0x0b,0xef, 0x80,0x44,0x6b, 0xff,0xff,0xff, 0x5d]
writecommandFromAddressValues :: (Word32,[Word32]) -> [Word8]
writecommandFromAddressValues (address,values) = commandbytes ++ [checksumbyte]
    where
      valuebytesFromValue :: Word32 -> [Word8]
      valuebytesFromValue value = [v2,v0,v1]  -- Have no idea why ds30loader organizes it this way
          where
            v2 = fromIntegral $ (value `quot` 0x10000) `rem` 0x100
            v1 = fromIntegral $ (value `quot`   0x100) `rem` 0x100
            v0 = fromIntegral $ (value `quot`     0x1) `rem` 0x100
      addressbytes = [a2, a1, a0]
          where
            a = address `quot` 2              -- Memory is parallel 8 and 16 bit memories index by later
            a2 = fromIntegral $ (a `quot` 0x10000) `rem` 0x100
            a1 = fromIntegral $ (a `quot`   0x100) `rem` 0x100
            a0 = fromIntegral $ (a `quot`     0x1) `rem` 0x100
      valuesbytes = concatMap valuebytesFromValue values
      lengthbyte = fromIntegral $ length valuesbytes + 1
      commandbytes = addressbytes ++ [0x02,lengthbyte] ++ valuesbytes
      checksumbyte = - sum commandbytes


main :: IO ()
main = do
  [file] <- getArgs
  result <- parseFromFile hfParse file
  print (map (writecommandFromAddressValues . addressvaluesFromAddressOffsetValues 128 0xffffff . addressoffsetvaluesFromSparseValues 128) .
             sparsevaluesGroupRows 128 . sparsevaluesOverwrite . sparsevaluesSort <$> result)
