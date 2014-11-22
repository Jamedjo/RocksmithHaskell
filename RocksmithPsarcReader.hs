module RocksmithPsarcReader where

import RocksmithPsarcHelpers
import RocksmithPsarcHeader
import RocksmithCrypto
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Control.Monad
import Data.Binary
import Data.Binary.Get

readPsarc :: String -> IO (Either String [PsarcEntry])
readPsarc path = do
  headerOrError <- readPsarcHeader path
  return (do
      header <- headerOrError
      gr <- fst (readIndexFromHeaderResult header)
      return (result gr))

readIndexFromHeaderResult :: GetResult PsarcHeader -> (Either String (GetResult [PsarcEntry]), B.ByteString)
readIndexFromHeaderResult hr = readIndex (result hr) decryptAndMakeIndex (unconsumed hr)

readIndex :: PsarcHeader -> (B.ByteString -> a) -> B.ByteString -> (a, B.ByteString)
readIndex header f bs = process f $ B.splitAt (fromIntegral (entriesSize header)) bs
    where
        process :: (B.ByteString -> a)-> (B.ByteString, B.ByteString) -> (a, B.ByteString)
        process f (indexData,rest) = (f indexData, rest)

entriesSize :: PsarcHeader -> Int
entriesSize header = entrySize * (numEntries header)
  where entrySize = 30 -- size of entry in bytes

decryptAndMakeIndex :: B.ByteString -> Either String (GetResult [PsarcEntry])
decryptAndMakeIndex = makeIndex . (B.fromStrict . decryptPsarc . B.toStrict)

makeIndex :: B.ByteString -> Either String (GetResult [PsarcEntry])
makeIndex = runGetResultOrFail $ getListOf getEntry

getListOf :: Get a -> Get [a]
getListOf get = do
  empty <- isEmpty
  if empty
     then return []
     else do el <- get
             rest <- getListOf get
             return (el : rest)

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
