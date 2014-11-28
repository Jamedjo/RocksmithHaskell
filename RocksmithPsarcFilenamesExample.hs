import RocksmithPsarcReader
import RocksmithPsarcHelpers

main = globMapArgs printFilenames

printFilenames :: String -> IO ()
printFilenames path = do
  putStrLn path
  printErrorOrString $ fmap (show . filenames) (readPsarc path)
  putStrLn ""
