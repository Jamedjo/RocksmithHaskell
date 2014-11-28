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
  path <- ErrorT getPath
  p <- ErrorT $ readPsarc path
  firstJson <- ErrorT . return $ getFirstJson p
  liftIO $ fExtractEntry path p firstJson

getPath :: IO (Either String String)
getPath = fmap firstArg getArgs 
  where firstArg args = safeHead args "This example needs the path to a psarc file.\n\
                                      \It will extract the first json from that file to stdout"

getFirstJson :: Psarc -> Either String PsarcEntry
getFirstJson p = safeHead (jsonEntries p) ("No json files found in:\n" ++ (showFileNames p))
  where showFileNames = C.unpack . C.unlines . filenames

jsonEntries :: Psarc -> [PsarcEntry]
jsonEntries p = filter ((".json" `isSuffixOf`) . C.unpack . filename) (entries p)

safeHead (h:_) msg = Right h
safeHead [] msg = Left msg
