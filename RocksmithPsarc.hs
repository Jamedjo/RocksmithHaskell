module RocksmithPsarc where

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Word
import Data.Functor ((<$>))

-- Word32 is a 32bit unsigned integer
data PsarcHeader = PsarcHeader { magicNumber :: Word32 }

readPsarcHeader :: String -> IO PsarcHeader
readPsarcHeader path = do
  input <- B.readFile path
  return (runGet matchHeader input)

matchHeader :: Get PsarcHeader -- Maybe PsarcHeader
matchHeader = PsarcHeader <$> getWord32be