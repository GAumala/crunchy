module CatalogReader (
  getCatalogFilePath,
  lookupEpisode,
  parseCatalogFile
) where

import Data.List
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import System.FilePath.Posix

catalogDir = "src/catalog/"

getCatalogFilePath :: String -> IO (Maybe String)
getCatalogFilePath key = do
  let filepath = catalogDir ++  key ++ ".txt"
  fileExists <- doesFileExist filepath
  if fileExists then
    return $ Just filepath
    else do
      putStrLn $ key ++ " not found."
      return Nothing

parseCatalogFile:: String -> IO [(String, String)]
parseCatalogFile filename = do
  content <- readFile filename
  let episodeList = lines content
  return (map (break (== ' ')) episodeList)

listToOptionMsg :: [String] -> String
listToOptionMsg [] = ""
listToOptionMsg [x] = x ++ "?"
listToOptionMsg [x, xs] = x ++ ", or " ++ listToOptionMsg [xs]
listToOptionMsg (x:xs) = x ++ ", " ++ listToOptionMsg xs

lookupEpisode :: Maybe String -> [(String, String)] -> [(String, String)]
lookupEpisode key episodeList =
  if isNothing key then episodeList
  else lookupEpisode' (fromJust key) episodeList

lookupEpisode' :: String -> [(String, String)] -> [(String, String)]
lookupEpisode' key (x:xs) = if key == fst x then x:xs
  else lookupEpisode' key xs
lookupEpisode' key [] = []
