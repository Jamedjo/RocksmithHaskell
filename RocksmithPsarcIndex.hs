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

type PsarcIndex = [PsarcEntry]
buildPsarcIndex :: [PsarcEntryWithZipLengths] -> [C.ByteString] -> [PsarcEntry]
buildPsarcIndex es fs = zip fs es

hBuildIndexFromRaw :: Handle -> Int -> PsarcIndexRaw -> IO PsarcIndex
hBuildIndexFromRaw h maxBlockSize = build . entriesWithZipLengths
  where
    build (filenameEntry:entries) = do
      filenames <- hExtractEntry h maxBlockSize filenameEntry
      return (buildPsarcIndex entries (C.lines filenames))

buildIndexFromRaw :: B.ByteString -> Int -> PsarcIndexRaw -> PsarcIndex
buildIndexFromRaw fileData maxBlockSize = build . entriesWithZipLengths
  where
    build (filenameEntry:entries) = buildPsarcIndex entries (C.lines (fileFn filenameEntry))
    fileFn = extractEntry fileData maxBlockSize

entriesWithZipLengths :: PsarcIndexRaw -> [PsarcEntryWithZipLengths]
entriesWithZipLengths (PsarcIndexRaw ents zipLs) = zip ents (selectZipLengths indices zipLs)
  where indices = map zipLengthsIndex ents

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

-- Select a range of zipLengths starting at index and adding up to at least the desired unpacked length
selectZipLengths :: (Integral i) => [i] -> [a] -> [[a]]
selectZipLengths is as = szl is as 0
  where
    szl :: (Integral i) => [i] -> [a] -> Int -> [[a]]
    szl _ [] _ = []:[]
    szl [] as _ = as:[]
    szl (0:is) as p = szl is as p
    szl (i:is) as p = let (xs,ys) = splitAt ((fromIntegral i)-p) as
                        in xs : (szl is ys (p+(length xs)))
