module RocksmithPsarcReader
  ( Psarc
  , PsarcRaw(..)
  , filenames
  , entries
  , readPsarc
  , readPsarcRaw
  , fExtractEntry
  ) where

import RocksmithPsarcHelpers
import RocksmithPsarcHeader
import RocksmithPsarcIndex
import RocksmithPsarcEntry
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import System.IO (withFile, IOMode(ReadMode))
import Control.Monad.Error

data PsarcRaw = PsarcRaw
  { header :: PsarcHeader
  , index :: PsarcIndexRaw
  }

data Psarc = Psarc PsarcHeader [PsarcEntry]
entries (Psarc _ es) = es
pHeader (Psarc h _) = h
filenames = map filename . entries

readPsarc :: FilePath -> ErrorT String IO Psarc
readPsarc path = do
  pr <- readPsarcRaw path
  liftIO $ addFilenames path pr

addFilenames :: FilePath -> PsarcRaw -> IO Psarc
addFilenames path p = withFile path ReadMode $ pFromRaw path (header p) (index p)
pFromRaw path hdr idx h = hBuildIndexFromRaw h (blockSize hdr) idx >>= return . (Psarc hdr)

fExtractEntry :: FilePath -> Psarc -> PsarcEntry -> IO B.ByteString
fExtractEntry path p e = withFile path ReadMode $ \h -> hExtractEntry h ((blockSize . pHeader) p) (entry e)

readPsarcRaw :: FilePath -> ErrorT String IO PsarcRaw
readPsarcRaw path = fmap result $ ErrorT (runGetOnFile getPsarcRaw path)

getPsarcRaw :: Get PsarcRaw
getPsarcRaw = do
  header <- getHeader
  index <- getIndexRaw header
  return (PsarcRaw header index)
