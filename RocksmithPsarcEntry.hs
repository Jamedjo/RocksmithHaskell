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

data PsarcEntryWithZipLengths = PsarcEntryWithZipLengths PsarcEntryRaw [Word16]

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
extractEntry :: Handle -> Int -> PsarcEntryWithZipLengths -> IO B.ByteString
extractEntry h maxBlockSize (PsarcEntryWithZipLengths entry zipLengths) = fmap B.concat $ mapM (extractBlock h) tuples
  where
    tuples :: [(Int,Int, Word16)]
    tuples = zipWith3 offsetTuple (offsets realBlockSizes) realBlockSizes zipLengths
    realBlockSizes = map realBlockSize zipLengths
    offsetTuple offset rbs b = (offset, rbs,  b)
    offsets :: (Integral a) =>[a] -> [a]
    offsets = scanl (+) (fromIntegral (fileOffset entry))
    realBlockSize :: (Integral l) => l -> Int
    realBlockSize l = if l==0 then maxBlockSize else (fromIntegral l)

extractBlock :: (Integral b) => Handle -> (Int,Int,b) -> IO B.ByteString
extractBlock h (offset, blockSize, storedBlockSize) = do
  hSeek h AbsoluteSeek (fromIntegral offset)
  rawData <- B.hGet h (fromIntegral blockSize)
  return $! process storedBlockSize rawData
  where
    process sbs d = (processFn sbs d) d
    processFn :: (Integral a) => a -> B.ByteString -> (B.ByteString -> B.ByteString)
    processFn storedBlockSize d
      | storedBlockSize == 0 = id
      | isZlib d = decompress
      | otherwise = id
    isZlib :: B.ByteString -> Bool
    isZlib d = (B.pack [0x78, 0xDA]) == (B.take 2 d)


