module Lib
    ( watchFromArgs
    ) where

import qualified Data.Map as Map
import Control.Monad
import Data.Foldable
import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import Livestreamer
import CatalogReader


-- Parses the catalog file for the requested show and resolves the episode to
-- stream. Once it gets a valid URL, starts the stream and prints an exit
-- message after the stream ends
watchEpisode :: Maybe String -> String -> IO ()
watchEpisode ep catalogPath = do
  catalog <- parseCatalogFile catalogPath
  let episodes = lookupEpisode ep catalog
  if null episodes then
    -- Using fromJust throws an exception if ep is Nothing. However, if ep is Nothing
    -- lookupEpisode returns all the available episodes, which should never be an empty list
    -- This line is meant fo when the user requests an inexistant episode
    putStrLn $ "episode " ++ fromJust ep ++ " not found."
  else do
    -- The episode URL will have one space at the beginning, let's trim that
    let episodeUrl = drop 1 $ snd $ head episodes
    result <- stream episodeUrl Nothing
    let livestreamerFinalMessage = last $ lines result
    putStrLn $ streamResult livestreamerFinalMessage

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
lookingUpEpisodeMessage showKey Nothing = "Looking up first episode of " ++
  showKey ++ "..."
lookingUpEpisodeMessage showKey (Just ep) = "Looking up episode " ++ ep ++ " of " ++
  showKey ++ "..."

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
    let (showKey, episode) = getEpisodeFromArgs args
    putStrLn $ lookingUpEpisodeMessage showKey episode
    catalogPath <- getCatalogFilePath showKey
    Data.Foldable.forM_ catalogPath (watchEpisode episode)
