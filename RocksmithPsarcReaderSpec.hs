import RocksmithPsarcReader
import Test.Hspec
import System.Environment (getArgs, withArgs)
import qualified Data.ByteString.Char8 as C
import Data.Either.Unwrap

main = do
  args <- getArgs
  main' args

main' (psarcPath:hspecArgs) = withArgs hspecArgs (testReadPsarc psarcPath)
main' _  = putStrLn "This test needs the path to a psarc file as an argument"

testReadPsarc psarcPath = hspec $ do
  describe "readPsarc" $ do
    it "can read a real file" $ do
      readPsarc psarcPath >>= (`shouldBe` Right (C.pack ""))
      --readPsarc psarcPath >>= (`shouldSatisfy` isRight)
