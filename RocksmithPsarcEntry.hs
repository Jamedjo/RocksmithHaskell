module RocksmithPsarcEntry where

import RocksmithPsarcHelpers
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Codec.Compression.Zlib (decompress)
import System.IO (Handle, hSeek, SeekMode(AbsoluteSeek))

data PsarcEntryRaw = PsarcEntryRaw
  { md5 :: B.ByteString
  , zipLengthsIndex :: Word32
  , unpackedLength :: Word64
  , fileOffset :: Word64
  }
  deriving (Show, Eq)

type PsarcEntryWithZipLengths = (PsarcEntryRaw, [Word16])

--data PsarcEntry = PsarcEntry (Maybe String) PsarcEntryRaw [Word16]
--contents :: PsarcEntry -> cacheFunction -> ByteString

getEntry :: Get PsarcEntryRaw
getEntry = do
  md5 <- getLazyByteString 16
  zipLengthsIndex <- getWord32be
  unpackedLength <- getWord40beAs64
  fileOffset <- getWord40beAs64
  return $! PsarcEntryRaw md5 zipLengthsIndex unpackedLength fileOffset

-- | Extracts an entry from the psarc file. If this file has been compressed with
-- zlib, then it will be slow. If you plan to reuse this data you should cache it.
hExtractEntry :: Handle -> Int -> PsarcEntryWithZipLengths -> IO B.ByteString
hExtractEntry h maxBlockSize (entry, zipLengths) = do
  hSeek h AbsoluteSeek (fromIntegral (fileOffset entry) )
  rawData <- B.hGet h (fromIntegral blockSizeSum)
  return $! extractEntryAtOffset rawData maxBlockSize (entry, zipLengths)
  where blockSizeSum = sum $ map (realBlockSize maxBlockSize) zipLengths

extractEntry :: B.ByteString -> Int -> PsarcEntryWithZipLengths -> B.ByteString
extractEntry input maxBs entry = extractEntryAtOffset (B.drop ((fromIntegral . fileOffset . fst) entry) input) maxBs entry

extractEntryAtOffset :: B.ByteString -> Int -> PsarcEntryWithZipLengths -> B.ByteString
extractEntryAtOffset input maxBs (entry, zipLengths) = B.concat $ map process $ fixSizes takeChunks
  where
    takeChunks = reverse . fst . (foldl stateSplit ([], input))
    stateSplit (acc, state) zl = (\(x, newstate) -> ((x:acc), newstate)) $ B.splitAt (fromIntegral zl) state
    fixSizes f = zip zipLengths $ f $ map (realBlockSize maxBs) zipLengths
    process ::  (Word16, B.ByteString) -> B.ByteString
    process (storedBlockSize, d)
      | storedBlockSize == 0 = d
      | isZlib d = decompress d
      | otherwise = d
    isZlib :: B.ByteString -> Bool
    isZlib d = (0x78 == (B.head d)) && ((runGet getWord16be $ B.take 2 d) `mod` 31) == 0

realBlockSize :: (Integral l) => Int -> l -> Int
realBlockSize maxBs l = if l==0 then maxBs else (fromIntegral l)