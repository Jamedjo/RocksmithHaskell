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

--need to change this to split at  * numEntries as not all of the index is entries
readIndex :: PsarcHeader -> (B.ByteString -> a) -> B.ByteString -> (a, B.ByteString)
readIndex header f bs = process f $ B.splitAt (fromIntegral (entriesSize header)) bs
    where
        process :: (B.ByteString -> a)-> (B.ByteString, B.ByteString) -> (a, B.ByteString)
        process f (indexData,rest) = (f indexData, rest)

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
  , zIndex :: Word32
  , eLength :: Word64
  , offset :: Word64
  }
  deriving (Show, Eq)

getEntry :: Get PsarcEntry
getEntry = do
  md5 <- getLazyByteString 16
  zIndex <- getWord32be
  eLength <- getWord40beAs64
  offset <- getWord40beAs64
  return $! PsarcEntry md5 zIndex eLength offset
