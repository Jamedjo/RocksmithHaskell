import RocksmithPsarcReader
import RocksmithPsarcEntry
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment (getArgs)
import System.FilePath.Glob
import Control.Monad
import Control.Monad.Error
import Data.List
import System.IO (stderr, hPutStrLn)

printErrIO :: ErrorT String IO C.ByteString -> IO ()
printErrIO et = runErrorT et >>= (either (hPutStrLn stderr) C.putStrLn)

main = printErrIO $ do
  args <- liftIO getArgs
  path <- ErrorT $ return $ safeHead args "This example needs the path to a psarc file.\n\
                           \It will extract the first json from that file to stdout"
  p <- ErrorT $ readPsarc path
  firstJson <- ErrorT $ return $ safeHead (jsonEntries p) ("No json files found in:\n" ++ (showFileNames p))
  liftIO $ fExtractEntry path p firstJson
  where
    jsonEntries :: Psarc -> [PsarcEntry]
    jsonEntries p = filter ((".json" `isSuffixOf`) . C.unpack . filename) (entries p)
    showFileNames = C.unpack . C.unlines . filenames

safeHead (h:_) msg = Right h
safeHead [] msg = Left msg
