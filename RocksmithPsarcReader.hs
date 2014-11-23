module RocksmithPsarcReader where

import RocksmithPsarcHelpers
import RocksmithPsarcHeader
import RocksmithCrypto
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Control.Monad
import Data.Binary
import Data.Binary.Get

data Psarc = Psarc
  { header :: PsarcHeader
  , index :: PsarcIndex
  }

data PsarcIndex = PsarcIndex
  { entries :: [PsarcEntry]
  , zipLengths :: B.ByteString
  }

readPsarc :: String -> IO (Either String Psarc)
readPsarc path = (fmap.fmap) result (runGetOnFile getPsarc path)

matchPsarc :: B.ByteString -> Either String (GetResult Psarc)
matchPsarc = runGetResultOrFail getPsarc

getPsarc :: Get Psarc
getPsarc = do
  header <- getHeader
  index <- getIndex header
  return (Psarc header index)

getIndex :: PsarcHeader -> Get PsarcIndex
getIndex ph = do
  encryptedIndex <- getByteString (indexSize ph)
  let indexData = B.fromStrict (decryptPsarc encryptedIndex)
  let index = runGet (getIndexFromUnencrypted ph) indexData
  return index

getIndexFromUnencrypted :: PsarcHeader -> Get PsarcIndex
getIndexFromUnencrypted ph = do
  entries <- getListOfN (numEntries ph) getEntry
  zipLengths <- getRemainingLazyByteString --getLazyByteString (fromIntegral ((indexSize ph) - (entriesSize ph)))
  return (PsarcIndex entries zipLengths)

entriesSize :: PsarcHeader -> Int
entriesSize header = entrySize * (numEntries header)
  where entrySize = 30 -- size of entry in bytes

getListOfN :: Int -> Get a -> Get [a]
getListOfN = replicateM

data PsarcEntry = PsarcEntry
  { md5 :: B.ByteString
  , zipLengthsIndex :: Word32
  , unpackedLength :: Word64
  , fileOffset :: Word64
  }
  deriving (Show, Eq)

getEntry :: Get PsarcEntry
getEntry = do
  md5 <- getLazyByteString 16
  zipLengthsIndex <- getWord32be
  unpackedLength <- getWord40beAs64
  fileOffset <- getWord40beAs64
  return $! PsarcEntry md5 zipLengthsIndex unpackedLength fileOffset
