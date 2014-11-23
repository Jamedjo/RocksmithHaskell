module RocksmithPsarcEntry where

import RocksmithPsarcHelpers
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get

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
