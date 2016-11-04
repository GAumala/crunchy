module LocalStorage (
  storeValue,
  readValue,
  readValueIfNecessary
) where

import System.Directory
_storageDir = "/.local/share/crunchy/db/"

getFilepath :: String -> String -> String
getFilepath homeDir key = homeDir ++ _storageDir ++ key ++ ".dat"

storeValue:: String -> String -> IO ()
storeValue key value = do
  homeDir <- getHomeDirectory
  writeFile (getFilepath homeDir key) value

readValue:: String -> IO (Maybe String)
readValue key = do
  homeDir <- getHomeDirectory
  let filepath = getFilepath homeDir key
  fileExists <- doesFileExist filepath
  if fileExists then do
    contents <- readFile filepath
    -- use takeWhile to remove any trailing newline characters
    return $ Just $ takeWhile (/= '\n') contents
  else
    return Nothing

readValueIfNecessary:: String -> Maybe String -> IO (Maybe String)
readValueIfNecessary showKey Nothing = readValue showKey
readValueIfNecessary showKey (Just ep) = return $ Just ep
