import RocksmithPsarcEntry
import Test.Hspec
import Codec.Compression.Zlib (compress)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary

block1 = C.pack "First part of test"
block2 = C.pack ". And the second part."
block2c = compress block2
block3 = C.pack "x\156\211Sp\204- Not really a zlib as it has a zipLength/blockSize of 0!!"
block4 = B.concat [C.pack "hi now", B.drop 6 block3]
len1 = B.length block1
len2 = B.length block2
len3 = B.length block3
len4 = len3
zipLengths = map $ fromIntegral . B.length

main = hspec $ do
  describe "extractEntry" $ do
    it "skips to the fileOffset" $
      B.length (testExtract [block1] len1) - len1 `shouldBe` 0
    it "uses zipLengths to split data" $
      (testZipLength [1,3] [block1] len1) `shouldBe` B.take 4 block1
    it "detects uncompressed blocks and uses them" $
      (testExtract [block1] len1) `shouldBe` block1
    it "detects decompress blocks and extracts them" $
      (testExtract [block2c] len2) `shouldBe` block2
    it "extracts multiblock parts" $
      (testExtract [block1,block2c] (len1+len2)) `shouldBe` B.concat [block1,block2]
    it "uses maxBlockSize when zipLength is 0" $
      (testZipLength [0,fromIntegral len1] [block4,block1] (len1+len4)) `shouldBe` B.concat [block4,block1]
    it "recognizes blocks with zipLength 0 as uncompressed" $
      (testZipLength [0,fromIntegral len1] [block3,block1] (len1+len3)) `shouldBe` B.concat [block3,block1]

maxBlockSize :: Int
maxBlockSize = fromIntegral (B.length block3)
dummyLength = 29
startData = B.pack $ take dummyLength $ repeat 0x00
entryDouble blocks dataLen = (entry, (zipLengths blocks))
  where entry = (PsarcEntryRaw B.empty 0 (fromIntegral dataLen) (fromIntegral dummyLength))
extractEntryHelper f bs dl = extractEntry (B.concat (startData:bs)) maxBlockSize (f (entryDouble bs dl))
testExtract = extractEntryHelper id
testZipLength zl = extractEntryHelper (\(e, _) -> (e, zl))
