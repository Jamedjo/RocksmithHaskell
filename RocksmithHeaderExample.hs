import RocksmithPsarcHeader
import RocksmithPsarcHelpers
import Control.Monad

main = globMapArgs printPathAndHeader

printPathAndHeader :: String -> IO ()
printPathAndHeader path = putStrLn path >> (readAndPrintHeader path) >> putStrLn ""

readAndPrintHeader :: String -> IO ()
readAndPrintHeader a = (readPsarcHeader a) >>= showHeaderOrError

showHeaderOrError :: Either String (GetResult PsarcHeader) -> IO ()
showHeaderOrError (Left msg) = putStrLn msg
showHeaderOrError (Right headerResult) = putStrLn (show (result headerResult))