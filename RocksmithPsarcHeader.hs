module RocksmithPsarcHeader (
              PsarcHeader,
              indexSize,
              entriesSize,
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
import System.IO.Error (tryIOError)
import Data.Bits (shiftR)
import RocksmithPsarcHelpers


data PsarcHeaderInternal = PsarcHeaderInternal {
    magicNumber :: String
  , header :: PsarcHeader
  }
  deriving (Show, Eq)

psarcHeaderInternalFromWords :: Word32 -> PsarcHeader -> PsarcHeaderInternal
psarcHeaderInternalFromWords = PsarcHeaderInternal . wordToString

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

entriesSize header = 30 * (numEntries header)

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
readPsarcHeader = fmap (>>= matchHeader) . tryRead
  where
    tryRead :: String -> IO (Either String B.ByteString)
    tryRead path = tryIOErrorString (B.readFile path)
    tryIOErrorString = (fmap eitherErrorToString) . tryIOError
    eitherErrorToString :: (Show e) => Either e a -> Either String a
    eitherErrorToString (Left e) = Left (show e)
    eitherErrorToString (Right a) = Right a

matchHeader :: B.ByteString -> Either String (GetResult PsarcHeader)
matchHeader input = validateHeader (getHeaderInternal input)

getHeaderInternal :: B.ByteString -> Either String (GetResult PsarcHeaderInternal)
getHeaderInternal = runGetResultOrFail getHeader

getHeader :: Get PsarcHeaderInternal
getHeader = do
  magicNumber <- getWord32be
  version <- getWord32be
  compressionMethod <- getWord32be
  totalTOCSize <- getWord32be
  tocEntrySize <- getWord32be
  numEntries <- getWord32be
  blockSize <- getWord32be
  archiveFlags <- getWord32be
  return $! psarcHeaderInternalFromWords magicNumber (psarcHeaderFromWords version compressionMethod totalTOCSize numEntries blockSize archiveFlags)

validateHeader :: Either String (GetResult PsarcHeaderInternal) -> Either String (GetResult PsarcHeader)
validateHeader headerInternal = (fmap header) <$> do
  hi <- headerInternal
  hi <- rejectUnless validFileHeader "Not a valid Psarc file" hi
  hi <- rejectUnless knownFileVersion "Unknown PSARC version" hi
  hi <- rejectUnless zlibCompression "Unknown compression method" hi
  return hi

rejectUnless :: (a -> Bool) -> s -> GetResult a -> Either s (GetResult a)
rejectUnless condition msg hr
    | condition (result hr) = Right hr
    | otherwise = Left msg

validFileHeader :: PsarcHeaderInternal -> Bool
validFileHeader PsarcHeaderInternal {magicNumber=mn} = mn == "PSAR"

knownFileVersion :: PsarcHeaderInternal -> Bool
knownFileVersion PsarcHeaderInternal {header=(PsarcHeader {version=v})} = v == PsarcVersion 1 4

zlibCompression :: PsarcHeaderInternal -> Bool
zlibCompression PsarcHeaderInternal {header=(PsarcHeader {compressionMethod=c})} = c == "zlib"
