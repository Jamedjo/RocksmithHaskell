import RocksmithPsarcReader
import RocksmithPsarcEntry
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment (getArgs)
import System.FilePath.Glob
import Control.Monad
import Data.List
import System.IO (stderr, hPutStrLn)

main = do
  args <- getArgs
  case args of
    (path:_) -> printFirstJson path
    otherwise -> putStrLn "This example needs the path to a psarc file.\n\
                          \It will extract the first json from that file to stdout"

printFirstJson :: FilePath -> IO ()
printFirstJson path = do
  p <- readPsarc path
  case p of 
    (Left msg) -> hPutStrLn stderr msg
    (Right p) -> extractFirstJson path p >>= C.putStrLn

extractFirstJson :: FilePath -> Psarc -> IO B.ByteString
extractFirstJson path p = fExtractEntry path p (firstJson jsonEntries)
  where
    firstJson (h:_) = h
    firstJson [] = error ("No json files found in " ++ C.unpack (C.unwords (filenames p)))
    jsonEntries = filter ((".json" `isSuffixOf`) . C.unpack . filename) (entries p)


