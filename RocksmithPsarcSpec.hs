import RocksmithPsarc
import Test.Hspec
import System.Environment (getArgs, withArgs)
import qualified Data.ByteString.Lazy as B
import Test.Hspec.Expectations.Contrib

main = do
  args <- getArgs
  main' args

main' (psarcPath:hspecArgs) = withArgs hspecArgs (testReadPsarc psarcPath)
main' _  = putStrLn "This test needs the path to a psarc file as an argument"

zeroBytes n = take n $ repeat 0x00
zeroWords n = zeroBytes (4*n)

dummyFile = B.pack $ [0x50, 0x53, 0x41, 0x52] ++ (zeroWords 1)
emptyFile = B.pack $ zeroWords 2

testReadPsarc psarcPath = hspec $ do
  describe "matchHeader" $ do
    it "extracts the psarc magic number from the file header" $
      getHeaderInternal dummyFile `shouldSatisfy` isRight
    it "fails if file doesn't have a psarc header" $
      matchHeader emptyFile `shouldBe` Left "Not a valid Psarc file"
    it "reads a PsarcHeader from valid files" $
       matchHeader dummyFile `shouldSatisfy` isRight
