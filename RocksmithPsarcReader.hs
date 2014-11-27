module RocksmithPsarcReader
  ( Psarc
  , PsarcRaw(..)
  , filenames
  , readPsarc
  , readPsarcRaw
  ) where

import RocksmithPsarcHelpers
import RocksmithPsarcHeader
import RocksmithPsarcIndex
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import System.IO (withFile, IOMode(ReadMode))

data PsarcRaw = PsarcRaw
  { header :: PsarcHeader
  , index :: PsarcIndexRaw
  }

data Psarc = Psarc PsarcHeader PsarcIndex
filenames (Psarc _ (PsarcIndex _ fs)) = fs

readPsarc :: String -> IO (Either String Psarc)
readPsarc path = readPsarcRaw path >>= eitherInIo (addFilenames path)
  where
    eitherInIo f = either (return . Left) (fmap Right . f)

addFilenames :: String -> PsarcRaw -> IO Psarc
addFilenames path p = withFile path ReadMode $ pFromRaw path (header p) (index p)
pFromRaw path hdr idx h = buildIndexFromRaw (blockSize hdr) idx h >>= return . (Psarc hdr)

readPsarcRaw :: String -> IO (Either String PsarcRaw)
readPsarcRaw path = (fmap.fmap) result (runGetOnFile getPsarcRaw path)

getPsarcRaw :: Get PsarcRaw
getPsarcRaw = do
  header <- getHeader
  index <- getIndexRaw header
  return (PsarcRaw header index)
