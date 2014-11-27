module RocksmithPsarcIndex where

import RocksmithCrypto
import RocksmithPsarcHeader
import RocksmithPsarcEntry
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary
import Data.Binary.Get
import Control.Monad
import Data.List.Split (splitPlaces)
import Control.Applicative
import System.IO (Handle)

data PsarcIndexRaw = PsarcIndexRaw [PsarcEntryRaw] [Word16]
rawEntries (PsarcIndexRaw es _) = es
zipLengths (PsarcIndexRaw _ zls) = zls

data PsarcIndex = PsarcIndex [PsarcEntryWithZipLengths] [C.ByteString]

buildIndexFromRaw :: Int -> PsarcIndexRaw -> Handle -> IO PsarcIndex
buildIndexFromRaw maxBlockSize (PsarcIndexRaw rawEntries zipLengths) h =
  indexFromEntries $ entriesWithZipLengths rawEntries zipLengths
  where
    indexFromEntries :: [PsarcEntryWithZipLengths]  -> IO PsarcIndex
    indexFromEntries (filenameEntry:entries) = extractFilenames filenameEntry >>= return . PsarcIndex entries
    extractFilenames :: PsarcEntryWithZipLengths -> IO [C.ByteString]
    extractFilenames filenameEntry = fmap C.lines $ hExtractEntry h maxBlockSize filenameEntry

getIndexRaw :: PsarcHeader -> Get PsarcIndexRaw
getIndexRaw ph = do
  encryptedIndex <- getByteString (indexSize ph)
  let indexData = B.fromStrict (decryptPsarc encryptedIndex)
  let index = runGet (getIndexFromUnencrypted ph) indexData
  return index

getIndexFromUnencrypted :: PsarcHeader -> Get PsarcIndexRaw
getIndexFromUnencrypted ph = do
  entries <- getListOfN (numEntries ph) getEntry
  zipLengths <- getRemainingAsListOf getWord16be
  return (PsarcIndexRaw entries zipLengths)

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

entriesWithZipLengths :: [PsarcEntryRaw] -> [Word16] -> [(PsarcEntryRaw, [Word16])]
entriesWithZipLengths ents zipLs = zip ents (selectZipLengths indices zipLs)
  where
    indices = map zipLengthsIndex ents

-- Select a range of zipLengths starting at index and adding up to at least the desired unpacked length
selectZipLengths :: (Integral i) => [i] -> [a] -> [[a]]
selectZipLengths indices as = splitPlaces lengths as
  where
    lengths = let firsts = map fromIntegral $ compareConsecutive (flip (-)) indices
              in firsts ++ [((length as) - (sum firsts))]
    compareConsecutive :: (a -> a -> b) -> [a] -> [b]
    compareConsecutive f ls = map (uncurry f) $ zip <*> tail $ ls

