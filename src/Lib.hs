module Lib
    ( watchFromArgs
    ) where

import Control.Monad
import Data.List
import Data.Foldable
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import Livestreamer
import CatalogReader
import LocalStorage


-- Parses the catalog file for the requested show and resolves the episode to
-- stream. Once it gets a valid URL, starts the stream and prints an exit
-- message after the stream ends
watchEpisode :: Maybe String -> String -> String -> IO ()
watchEpisode ep showId catalogPath = do
  putStrLn $ lookingUpEpisodeMessage showId ep
  catalog <- parseCatalogFile catalogPath
  let episodes = lookupEpisode ep catalog
  if null episodes then
    -- Using fromJust throws an exception if ep is Nothing. However, if ep is Nothing
    -- lookupEpisode returns all the available episodes, which should never be an empty list
    -- This line is meant fo when the user requests an inexistant episode
    putStrLn $ "episode " ++ fromJust ep ++ " not found."
  else
    watchSequence showId episodes

handleEndOfShow :: String -> IO()
handleEndOfShow showId = do
  removeValue showId
  putStrLn $ "You have watched all available episodes of " ++ showId ++ ". Goodbye!"

onLivestreamerClose :: String -> String -> [(String, String)] -> IO ()
onLivestreamerClose showId consoleOutput nextEpisodes = do
  let exitMessage = last $ lines consoleOutput
  let success = streamFinishedSuccessfully exitMessage

  if success == Just True && nextEpisodes /= [] then
    saveSessionAndContinue showId nextEpisodes else
    if success == Just True && null nextEpisodes then
    handleEndOfShow showId else
    if success == Just False then
    putStrLn "Closed the stream. See you later!"
    else putStrLn $ "Something unexpected happened. livestreamer output:\n" ++ consoleOutput

-- Parses the catalog file for the requested show and streams an episode starting
-- from the requested position
watchUnfinishedEpisode :: String -> String -> String -> String -> IO ()
watchUnfinishedEpisode epId showId catalogPath startPos = do
  putStrLn $ lookingUpEpisodeMessage showId (Just epId)
  catalog <- parseCatalogFile catalogPath
  let (_, s:url):nextEpisodes = lookupEpisode (Just epId) catalog
  consoleOutput <- saveNewMPVScript showId epId >>= streamPreviousSession url Nothing startPos
  onLivestreamerClose showId consoleOutput nextEpisodes


watchSequence :: String -> [(String, String)] -> IO ()
  -- The episode URL will have one space at the beginning, let's trim that
watchSequence showId ((epId, s:url):nextEpisodes) = do
  -- save a lua script to persist progress and then start livestreamer
  consoleOutput <- saveNewMPVScript showId epId >>= stream url Nothing
  onLivestreamerClose showId consoleOutput nextEpisodes

saveSessionAndContinue :: String -> [(String, String)] -> IO ()
saveSessionAndContinue showId nextEps = do
  let nextEp = fst $ head nextEps
  --next episode should start at zero
  storeValue showId (nextEp ++ "\n0")
  putStrLn $ "Continuing with episode " ++ nextEp ++ "..."
  watchSequence showId nextEps

getEpisodeFromArgs :: [String] -> (String, Maybe String)
getEpisodeFromArgs args =
  let reverseArgs = reverse args in
  if length args >= 2 then
    -- (2nd last, last)
    (head $ tail reverseArgs, Just $ head reverseArgs)
  else
    -- (last, Nothing)
    (head reverseArgs , Nothing)

lookingUpEpisodeMessage :: String -> Maybe String -> String
lookingUpEpisodeMessage showId Nothing = "Looking up first episode of " ++
  showId ++ "..."
lookingUpEpisodeMessage showId (Just ep) = "Looking up episode " ++ ep ++ " of " ++
  showId ++ "..."

saveNewMPVScript :: String -> String -> IO String
saveNewMPVScript showId episodeId = do
  script <- getTemplateScript
  pathToFile <- getPathToDatFile showId
  let firstLine =  "pathToFile = \""++ pathToFile ++ "\""
  let secondLine = "episodeId = \"" ++  episodeId ++ "\""
  let customScript = intercalate "\n" [firstLine, secondLine, script]
  saveScript customScript

-- Parses command line arguments to start a livestreamer process and watch the
-- desired show. Currently the supported usage is:
-- crunchy SHOW_ID [EPISODE_ID]
-- If zero arguments are passed, exit with status code 1 and an appropiate error message
-- If only one argument is passed, it is assumed that it is SHOW_ID. program will
-- play the first episode of that show.
-- If more than 2 arguments are passed, it is assumed that the last argument is
-- the episode identifier and the second last is the show identifier
watchFromArgs :: IO ()
watchFromArgs = do
  args <- getArgs
  if null args then do
    hPutStrLn stderr $ "Error: You must pass an identifier of the show you " ++
      "want to watch and, optionally, an episode.\nCurrently the supported " ++
      "usage is:\n\n\tcrunchy SHOW_ID [EPISODE_ID]\n"
    exitFailure
  else do
    let (showId, _ep) = getEpisodeFromArgs args
    catalogPath <- getCatalogFilePath showId
    -- if user omits episode we should try to retrieve session from storage
    prevSession <- readValueIfNecessary showId _ep
    if isNothing _ep && isJust prevSession then do
      putStrLn "Restoring previous session..."
      let [episodeId, position] = lines $ fromJust prevSession
      watchUnfinishedEpisode episodeId showId (fromJust catalogPath) position
    else
    -- I don't really understand this line, linter suggested it. It is supposed to
    -- call (watchEpisode episode showId catalogPath) only if catalogPath is Just
      Data.Foldable.forM_ catalogPath (watchEpisode _ep showId)
