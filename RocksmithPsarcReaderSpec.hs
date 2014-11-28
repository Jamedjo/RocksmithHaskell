import RocksmithPsarcReader
import RocksmithPsarcEntry
import RocksmithPsarcIndex
import Test.Hspec
import System.Environment (getArgs, withArgs)
import Control.Applicative ((<*>))
import Control.Monad.Error

main = do
  args <- getArgs
  main' args

main' (psarcPath:hspecArgs) = withArgs hspecArgs (testReadPsarc psarcPath)
main' _  = putStrLn "This test needs the path to a psarc file as an argument"

testReadPsarc psarcPath = hspec $ do
  describe "readPsarcRaw on a real file" $ do
    it "contains many PsarcEntries" $
      shouldSatisfyRightOfF readPsarcRaw (length . rawEntries . index) (>0)
    it "has increasing zipLengthsIndex" $
      shouldSatisfyRightOfF readPsarcRaw ((map zipLengthsIndex) . rawEntries . index) increasing
    it "has increasing fileOffsets" $
      shouldSatisfyRightOfF readPsarcRaw ((map fileOffset) . rawEntries . index) increasing
    it "has more zipLengths than entries" $
      shouldSatisfyRightOfF readPsarcRaw index ((length.zipLengths) $>= (length.rawEntries))
        
  describe "readPsarc" $ do
    it "should extract filenames" $
      shouldSatisfyRightOfF readPsarc (length . filenames) (>1)
  where
    ($>=) = compareFn (>=)
    compareFn c left right b = (left b) `c` (right b)
    shouldBeRightOfF f l r = runErrorT (fmap l (f psarcPath)) >>= (`shouldBe` Right r)
    shouldSatisfyRightOfF f l r =  shouldBeRightOfF f (r . l) True

pairwise = zip <*> tail
increasing xs = all (uncurry (<)) $ pairwise xs
