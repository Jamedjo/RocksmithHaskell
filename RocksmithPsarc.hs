module RocksmithPsarc (
              readPsarcHeader,
              matchHeader,
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

-- Word32 is a 32bit unsigned integer

data PsarcVersion = PsarcVersion { major :: Int, minor :: Int}
  deriving (Eq)
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

instance Show PsarcVersion where
  show (PsarcVersion major minor) = (show major)++"."++(show minor)

newtype CompressionMethod = CompressionMethod Word32
  deriving (Show, Eq)

data PsarcHeader = PsarcHeader
  {
     version :: PsarcVersion
  ,  compressionMethod :: CompressionMethod
  }
  deriving (Show, Eq)

data PsarcHeaderInternal = PsarcHeaderInternal {
    magicNumber :: Word32
  , header :: PsarcHeader
  }
  deriving (Show, Eq)

readPsarcHeader :: String -> IO (Either String PsarcHeader)
readPsarcHeader = fmap (>>= matchHeader) . tryRead
  where
    tryRead :: String -> IO (Either String B.ByteString)
    tryRead path = tryIOErrorString (B.readFile path)
    tryIOErrorString = (fmap eitherErrorToString) . tryIOError
    eitherErrorToString :: (Show e) => Either e a -> Either String a
    eitherErrorToString (Left e) = Left (show e)
    eitherErrorToString (Right a) = Right a

matchHeader :: B.ByteString -> Either String PsarcHeader
matchHeader input = validateHeader (getHeaderInternal input)

getHeaderInternal :: B.ByteString -> Either String PsarcHeaderInternal
getHeaderInternal = eitherResult . runGetOrFail getHeader
  where
    eitherResult :: Either (t1, t2, a)  (t3, t4, b) -> Either a b
    eitherResult (Left (_,_,a)) = (Left a)
    eitherResult (Right (_,_,a)) = (Right a)

getHeader :: Get PsarcHeaderInternal
getHeader = do
  magicNumber <- getWord32be
  version <- getWord32be
  compressionMethod <- getWord32be
  totalTOCSize <- getWord32be
  tocEntrySize <- getWord32be
  numFiles <- getWord32be
  blockSize <- getWord32be
  archiveFlags <- getWord32be
  return $! PsarcHeaderInternal magicNumber (PsarcHeader (wordToPsarcVersion version) (CompressionMethod compressionMethod))

validateHeader :: Either String PsarcHeaderInternal -> Either String PsarcHeader
validateHeader headerInternal = header <$> do
  hi <- headerInternal
  hi <- rejectUnless validFileHeader "Not a valid Psarc file" hi
  --hi <- rejectUnless recognizedFileVersion "Unknown psarc version" hi
  --hi <- rejectUnless zlibCompression "Unknown compression method" hi
  return hi -- get main header from header internal

rejectUnless :: (a -> Bool) -> s -> a -> Either s a
rejectUnless condition msg a
    | condition a = Right a
    | otherwise = Left msg

validFileHeader :: PsarcHeaderInternal -> Bool
validFileHeader PsarcHeaderInternal {magicNumber=mn} = wordToString mn == "PSAR"
  where wordToString = C.unpack . encode
