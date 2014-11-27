import RocksmithPsarcReader
import System.Environment (getArgs)
import System.FilePath.Glob
import Control.Monad

main = globMapArgs printFilenames

globMapArgs f = getArgs >>= fmap join . mapM glob >>= mapM_ f

printFilenames :: String -> IO ()
printFilenames path = do
  putStrLn path
  p <- readPsarc path
  case p of 
    (Left msg) -> putStrLn msg
    (Right p) -> putStrLn (show (filenames p))
  putStrLn ""