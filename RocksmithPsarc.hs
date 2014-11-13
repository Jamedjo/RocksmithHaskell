module RocksmithPsarc where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary.Get
import Data.Binary
import Data.Word
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

-- Word32 is a 32bit unsigned integer

data PsarcHeader = PsarcHeader
  deriving (Show, Eq)

data PsarcHeaderInternal = PsarcHeaderInternal { magicNumber :: Word32, header :: PsarcHeader}
  deriving (Show, Eq)

readPsarcHeader :: String -> IO (Either String PsarcHeader)
readPsarcHeader path = matchHeader <$> B.readFile path


validFileHeader :: PsarcHeaderInternal -> Bool
validFileHeader PsarcHeaderInternal {magicNumber=mn} = wordToString mn == "PSAR"
  where wordToString = C.unpack . encode

rejectUnless :: (a -> Bool) -> s -> a -> Either s a
rejectUnless condition msg a
    | condition a = Right a
    | otherwise = Left msg

validateHeader :: Either String PsarcHeaderInternal -> Either String PsarcHeader
validateHeader headerInternal = header <$> do
  hi <- headerInternal
  hi <- rejectUnless validFileHeader "Not a valid Psarc file" hi
  --hi <- rejectUnless recognizedFileVersion "Unknown psarc version" hi
  --hi <- rejectUnless zlibCompression "Unknown compression method" hi
  return hi -- get main header from header internal

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
  return $! PsarcHeaderInternal magicNumber PsarcHeader