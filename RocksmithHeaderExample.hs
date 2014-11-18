import RocksmithPsarcHeader
import System.Environment (getArgs)
import System.FilePath.Glob
import Control.Monad

main = getArgs >>= fmap join . mapM glob >>= mapM_ printPathAndHeader

printPathAndHeader :: String -> IO ()
printPathAndHeader path = putStrLn path >> (readAndPrintHeader path) >> putStrLn ""

readAndPrintHeader :: String -> IO ()
readAndPrintHeader a = (readPsarcHeader a) >>= showHeaderOrError

showHeaderOrError :: Either String (HeaderResult PsarcHeader) -> IO ()
showHeaderOrError (Left msg) = putStrLn msg
showHeaderOrError (Right headerResult) = putStrLn (show (result headerResult))