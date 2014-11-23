module RocksmithPsarcIndex where

import RocksmithCrypto
import RocksmithPsarcHeader
import RocksmithPsarcEntry
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Control.Monad

data PsarcIndex = PsarcIndex
  { entries :: [PsarcEntry]
  , zipLengths :: B.ByteString
  }

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
