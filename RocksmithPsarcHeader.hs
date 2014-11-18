module RocksmithPsarcHeader (
              PsarcHeader,
              readPsarcHeader,
              matchHeader,
              HeaderResult,
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
  ,  tocSize :: Integer
  ,  numEntries :: Integer
  ,  blockSize :: Integer
  ,  archiveFlags :: Integer
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

readPsarcHeader :: String -> IO (Either String (HeaderResult PsarcHeader))
readPsarcHeader = fmap (>>= matchHeader) . tryRead
  where
    tryRead :: String -> IO (Either String B.ByteString)
    tryRead path = tryIOErrorString (B.readFile path)
    tryIOErrorString = (fmap eitherErrorToString) . tryIOError
    eitherErrorToString :: (Show e) => Either e a -> Either String a
    eitherErrorToString (Left e) = Left (show e)
    eitherErrorToString (Right a) = Right a

matchHeader :: B.ByteString -> Either String (HeaderResult PsarcHeader)
matchHeader input = validateHeader (getHeaderInternal input)

data HeaderResult a = HeaderResult (B.ByteString, ByteOffset, a)
  deriving (Show, Eq)
instance Functor HeaderResult where
  fmap f (HeaderResult (bs, bo, a)) = HeaderResult (bs, bo, f a)

result :: HeaderResult a -> a
result (HeaderResult (_,_,a)) = a

unconsumed :: HeaderResult a -> B.ByteString
unconsumed (HeaderResult (b,_,_)) = b

getHeaderInternal :: B.ByteString -> Either String (HeaderResult PsarcHeaderInternal)
getHeaderInternal = eitherResult . runGetOrFail getHeader
  where
    eitherResult :: Either (B.ByteString, ByteOffset, a) (B.ByteString, ByteOffset, b) -> Either a (HeaderResult b)
    eitherResult (Left (_,_,a)) = (Left a)
    eitherResult (Right (bs,bo,a)) = (Right (HeaderResult (bs,bo,a)))

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

validateHeader :: Either String (HeaderResult PsarcHeaderInternal) -> Either String (HeaderResult PsarcHeader)
validateHeader headerInternal = (fmap header) <$> do
  hi <- headerInternal
  hi <- rejectUnless validFileHeader "Not a valid Psarc file" hi
  hi <- rejectUnless knownFileVersion "Unknown PSARC version" hi
  hi <- rejectUnless zlibCompression "Unknown compression method" hi
  return hi

rejectUnless :: (a -> Bool) -> s -> HeaderResult a -> Either s (HeaderResult a)
rejectUnless condition msg hr
    | condition (result hr) = Right hr
    | otherwise = Left msg

validFileHeader :: PsarcHeaderInternal -> Bool
validFileHeader PsarcHeaderInternal {magicNumber=mn} = mn == "PSAR"

knownFileVersion :: PsarcHeaderInternal -> Bool
knownFileVersion PsarcHeaderInternal {header=(PsarcHeader {version=v})} = v == PsarcVersion 1 4

zlibCompression :: PsarcHeaderInternal -> Bool
zlibCompression PsarcHeaderInternal {header=(PsarcHeader {compressionMethod=c})} = c == "zlib"
