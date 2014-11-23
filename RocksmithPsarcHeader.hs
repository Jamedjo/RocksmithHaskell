module RocksmithPsarcHeader (
              PsarcHeader,
              indexSize,
              numEntries,
              readPsarcHeader,
              matchHeader,
              GetResult,
              result,
              unconsumed,
            ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary.Get
import Data.Binary
import Data.Word
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.Bits (shiftR)
import RocksmithPsarcHelpers

data PsarcHeader = PsarcHeader
  {
     version :: PsarcVersion
  ,  compressionMethod :: String
  ,  indexSize :: Int
  ,  numEntries :: Int
  ,  blockSize :: Int
  ,  archiveFlags :: Int
  }
  deriving (Show, Eq)

psarcHeaderFromWords :: (Integral ts, Integral ne, Integral bs, Integral af) => Word32 -> Word32 -> ts -> ne -> bs -> af -> PsarcHeader
psarcHeaderFromWords v cm ts ne bs af = PsarcHeader (wordToPsarcVersion v) (wordToString cm) (fromIntegral ts) (fromIntegral ne) (fromIntegral bs) (fromIntegral af)

wordToString :: Word32 -> String
wordToString = C.unpack . encode

data PsarcVersion = PsarcVersion { major :: Int, minor :: Int}
  deriving (Eq)

instance Show PsarcVersion where
  show (PsarcVersion major minor) = (show major)++"."++(show minor)

wordToPsarcVersion :: Word32 -> PsarcVersion
wordToPsarcVersion w = PsarcVersion (fromIntegral mj) (fromIntegral mn)
  where mj = octets w !! 1
        mn = octets w !! 3

octets :: Word32 -> [Word8]
octets w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

readPsarcHeader :: String -> IO (Either String (GetResult PsarcHeader))
readPsarcHeader = runGetOnFile getHeader

matchHeader :: B.ByteString -> Either String (GetResult PsarcHeader)
matchHeader = runGetResultOrFail getHeader

getHeader :: Get PsarcHeader
getHeader = do
  magicNumber <- getWord32be
  rejectUnless validFileHeader "Not a valid Psarc file" magicNumber

  version <- getWord32be
  compressionMethod <- getWord32be
  totalTOCSize <- getWord32be
  tocEntrySize <- getWord32be
  numEntries <- getWord32be
  blockSize <- getWord32be
  archiveFlags <- getWord32be

  let header = psarcHeaderFromWords version compressionMethod totalTOCSize numEntries blockSize archiveFlags
  rejectUnless knownFileVersion "Unknown PSARC version" header
  rejectUnless zlibCompression "Unknown compression method" header
  return $! header

rejectUnless :: (a -> Bool) -> String -> a -> Get ()
rejectUnless condition msg hr
    | condition hr = return ()
    | otherwise = fail msg

validFileHeader :: Word32 -> Bool
validFileHeader = (== "PSAR") . wordToString

knownFileVersion :: PsarcHeader -> Bool
knownFileVersion PsarcHeader {version=v} = v == PsarcVersion 1 4

zlibCompression :: PsarcHeader -> Bool
zlibCompression PsarcHeader {compressionMethod=c} = c == "zlib"
