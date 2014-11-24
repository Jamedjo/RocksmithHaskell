import RocksmithPsarcReader
import RocksmithPsarcEntry
import RocksmithPsarcIndex
import Test.Hspec
import System.Environment (getArgs, withArgs)
import Control.Applicative ((<*>))

main = do
  args <- getArgs
  main' args

main' (psarcPath:hspecArgs) = withArgs hspecArgs (testReadPsarc psarcPath)
main' _  = putStrLn "This test needs the path to a psarc file as an argument"

testReadPsarc psarcPath = hspec $ do
  describe "readPsarc can read a real file" $ do
    it "contains many PsarcEntries" $
      length . entries . index `shouldSatisfyRightOf` (>0)
    it "has increasing zipLengthsIndex" $
      (map zipLengthsIndex) . entries . index `shouldSatisfyRightOf` increasing
    it "has increasing fileOffsets" $
      (map fileOffset) . entries . index `shouldSatisfyRightOf` increasing
  where
    shouldBeRightOf l r =  (fmap.fmap) l (readPsarc psarcPath) >>= (`shouldBe` Right r)
    infix 1 `shouldBeRightOf`
    shouldSatisfyRightOf l r = shouldBeRightOf (r . l) True
    infix 1 `shouldSatisfyRightOf`

pairwise = zip <*> tail
increasing xs = all (uncurry (<)) $ pairwise xs
