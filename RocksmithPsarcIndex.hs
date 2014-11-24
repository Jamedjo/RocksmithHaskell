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
  , zipLengths :: [Word16]
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
  zipLengths <- getRemainingAsListOf getWord16be
  return (PsarcIndex entries zipLengths)

getListOfN :: Int -> Get a -> Get [a]
getListOfN = replicateM

getRemainingAsListOf :: Get a -> Get [a]
getRemainingAsListOf get = do
  empty <- isEmpty
  if empty
     then return []
     else do el <- get
             rest <- getRemainingAsListOf get
             return (el : rest)
