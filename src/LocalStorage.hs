module LocalStorage (
  getPathToDatFile,
  getScriptPath,
  getTemplateScript,
  readValue,
  readValueIfNecessary,
  removeValue,
  saveScript,
  storeValue
) where

import System.Directory

appDir = "/.local/share/crunchy/"
_storageDir = appDir  ++ "db/"

-- Get the path to writeable file with the first argument as base name
getPathToDatFile :: String -> IO String
getPathToDatFile name = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ _storageDir ++ name ++ ".dat"

getTemplateScript:: IO String
getTemplateScript = do
  homeDir <- getHomeDirectory
  readFile (homeDir ++ appDir ++ "mpv_template_script.lua")

getScriptPath :: IO String
getScriptPath = do
  homeDir <- getHomeDirectory
  return (homeDir ++ appDir ++ "crunchy.lua")

storeValue:: String -> String -> IO ()
storeValue key value = do
  path <- getPathToDatFile key
  writeFile path value

-- saves a script to a path readable by mpv
-- returns the path of the saved script
saveScript :: String ->  IO String
saveScript script = do
  path <- getScriptPath
  writeFile path script >> return path

readValue:: String -> IO (Maybe String)
readValue key = do
  path <- getPathToDatFile key
  fileExists <- doesFileExist path
  if fileExists then do
    contents <- readFile path
    return $ Just contents
  else
    return Nothing

removeValue:: String -> IO ()
removeValue key = do
  path <- getPathToDatFile key
  removeFile path

readValueIfNecessary:: String -> Maybe String -> IO (Maybe String)
readValueIfNecessary showKey Nothing = readValue showKey
readValueIfNecessary showKey (Just ep) = return $ Just ep
