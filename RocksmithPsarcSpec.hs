import RocksmithPsarc
import Test.Hspec
import System.Environment (getArgs, withArgs)
import qualified Data.ByteString.Lazy as B

main = do
  args <- getArgs
  main' args

main' (psarcPath:hspecArgs) = withArgs hspecArgs (testReadPsarc psarcPath)
main' _  = putStrLn "This test needs the path to a psarc file as an argument"

zeroBytes n = take n $ repeat 0x00
zeroWords n = zeroBytes (4*n)

isPsarcHeaderInternal :: Either String PsarcHeaderInternal -> Bool
isPsarcHeaderInternal (Right (PsarcHeaderInternal {})) = True
isPsarcHeaderInternal _ = False

testReadPsarc psarcPath = hspec $ do
  describe "matchHeader" $ do
    it "extracts the psarc magic number from the file header" $
      (getHeaderInternal $ B.pack ([0x50, 0x53, 0x41, 0x52] ++ (zeroWords 1))) `shouldSatisfy` isPsarcHeaderInternal
    it "fails if file doesn't have a psarc header" $
      (matchHeader $ B.pack $ zeroWords 2) `shouldBe` Left "Not a valid Psarc file"
      -- fmap (fromIntegral . magicNumber) (readPsarcHeader psarcPath) >>= (`shouldSatisfy` (>100))