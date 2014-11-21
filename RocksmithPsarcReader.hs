module RocksmithPsarcReader where

import RocksmithPsarcHeader
import RocksmithCrypto
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS


readPsarc :: String -> IO (Either String BS.ByteString)
readPsarc path = (readPsarcHeader path) >>= return . (fmap (fst . readIndexFromHeaderResult))

readIndexFromHeaderResult :: GetResult PsarcHeader -> (BS.ByteString, B.ByteString)
readIndexFromHeaderResult hr = readIndex (indexSizeInBytes (result hr)) (unconsumed hr)
    where indexSizeInBytes ph = indexSize ph

readIndex :: Integral s => s -> B.ByteString -> (BS.ByteString, B.ByteString)
readIndex indexSize bs = process $ B.splitAt (fromIntegral indexSize) bs
    where
        process :: (B.ByteString, B.ByteString) -> (BS.ByteString, B.ByteString)
        process (indexData,rest) = (makeIndex (decryptPsarc (B.toStrict indexData)), rest)

makeIndex = id
