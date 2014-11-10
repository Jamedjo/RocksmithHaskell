import RocksmithPsarc
import Test.Hspec
import System.Environment (getArgs, withArgs)

main = do
  args <- getArgs
  main' args

main' (psarcPath:hspecArgs) = withArgs hspecArgs (testReadPsarc psarcPath)
main' _  = putStrLn "This test needs the path to a psarc file as an argument"


testReadPsarc psarcPath = hspec $ do
  describe "readPsarcHeader" $ do
    it "reads the header from a psarc file" $
      fmap (fromIntegral . magicNumber) (readPsarcHeader psarcPath) >>= (`shouldSatisfy` (>100))