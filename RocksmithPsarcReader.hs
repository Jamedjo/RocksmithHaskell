module RocksmithPsarcReader where

import RocksmithPsarcHelpers
import RocksmithPsarcHeader
import RocksmithPsarcIndex
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get

data Psarc = Psarc
  { header :: PsarcHeader
  , index :: PsarcIndex
  }

readPsarc :: String -> IO (Either String Psarc)
readPsarc path = (fmap.fmap) result (runGetOnFile getPsarc path)

getPsarc :: Get Psarc
getPsarc = do
  header <- getHeader
  index <- getIndex header
  return (Psarc header index)
