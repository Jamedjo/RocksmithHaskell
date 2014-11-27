import RocksmithPsarcEntry
import RocksmithPsarcIndex
import Test.Hspec
import Codec.Compression.Zlib (compress)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

main = hspec $ do
  describe "selectZipLengths" $ do
    it "uses indicies to group elements" $
      map length (selectZipLengths indices testZipLengths) `shouldBe` [1,1,2,2,8,1,17]
    it "gets the right indicies for dummy data" $
      selectZipLengths fileIndicies (zls fileData) `shouldBe` (map.map) (fromIntegral . B.length) fileDataGroups

  describe "buildIndexFromRaw" $ do
    it "extracts a list of file names" $
      head (getFilenames buildIndex) `shouldBe` C.pack "happy.sng"
    it "passes correct indices to entries and joins them" $
      splitAt 1 (map (extractEntry (B.concat fileData) 64) (getEntries buildIndex)) `shouldBe` ([B.concat [entry2a, entry2b]], [entry3])
  where
    buildIndex = buildHelper entries fileData
      where entries = map (uncurry entryDouble) $ zip fileIndicies fileOffsets
    fileData = concat fileDataGroups
    fileDataGroups = [[filenames], [entry2a, entry2b], [entry3]]
    fileIndicies = scanSumLength length fileDataGroups
    fileOffsets = scanSumLength (B.length . B.concat) fileDataGroups
    scanSumLength f gs = init $ scanl (+) 0 $ map (fromIntegral . f) gs
    filenames = compress $ C.pack "happy.sng\nhappy.json\nred-green.txt\nimage.dds"
    entry2a = C.pack "Just som"
    entry2b = C.pack "e dummy data."
    entry3 = C.pack "{'success':'true'}"

entryDouble i off = PsarcEntryRaw B.empty i 0 off
buildHelper :: [PsarcEntryRaw] -> [B.ByteString] -> PsarcIndex
buildHelper es bs = buildIndexFromRaw (B.concat bs) 64 $ PsarcIndexRaw es (zls bs)
zls = map $ fromIntegral . B.length
getFilenames = map filename
getEntries = map entry

--it should deal with seek failure

indices = [0,1,2,4,6,14,15]
testZipLengths = [268,216,63111,25719,11772,6392,0,0,0,0,0,0,0,10375,6,1298,47710,33971,21069,836,48571,31664,35850,38708,64697,27832,52268,30868,46682,23672,45556,56315]
