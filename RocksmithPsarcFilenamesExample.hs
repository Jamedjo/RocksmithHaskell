import RocksmithPsarcReader
import RocksmithPsarcHelpers
import Control.Monad

main = globMapArgs printFilenames

printFilenames :: String -> IO ()
printFilenames path = do
  putStrLn path
  p <- readPsarc path
  case p of 
    (Left msg) -> putStrLn msg
    (Right p) -> putStrLn (show (filenames p))
  putStrLn ""