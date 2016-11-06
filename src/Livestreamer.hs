module Livestreamer (
  stream,
  streamFinishedSuccessfully,
  streamPreviousSession,
  streamResult
) where

import Data.Maybe
import Data.List
import System.IO
import System.Process

mpvCmd script moreArgs = "mpv --cache=8192 --fullscreen --script=" ++ script ++
  " " ++ moreArgs

stream' :: String -> Maybe String -> String -> String -> IO String
stream' episodeUrl quality script moreArgs = do
    (code, stdout, stderr) <- readProcessWithExitCode "livestreamer"
      ["--player", mpvCmd script moreArgs, "--verbose-player", episodeUrl,
      fromMaybe "ultra" quality] ""
    return stdout


-- Starts a new livestreamer process to stream an episode.
-- Receives 3 String arguments:
-- the episode's crunchyroll URL
-- the path to the lua script that will persist playback position on quit
-- the quality of stream. if quality is Nothing defaults to ultra quality.
--
--  Returns a String with livestreamer's exit message since exit code is always 0
-- To ensure that the exit message is always printed, starts the process with
-- verbose-player flag.
stream episodeUrl quality script = stream' episodeUrl quality script []

-- Starts a new livestreamer process to stream an episode adding an extra
-- command line parameter to start from a requested position.
-- Receives r String arguments:
-- the episode's crunchyroll URL
-- the path to the lua script that will persist playback position on quit
-- the quality of stream. if quality is Nothing defaults to ultra quality.
-- the start position.
--
--  Returns a String with livestreamer's exit message since exit code is always 0
-- To ensure that the exit message is always printed, starts the process with
-- verbose-player flag.
streamPreviousSession episodeUrl quality startPos script = stream' episodeUrl
  quality script ("--start=+" ++ startPos)

-- Reads the final message from the livestreamer process's stdout to conclude
-- wether the stream finished successfully or not.
-- This function is tightly coupled to livestreamer and probably mpv.
-- Should livestreamer print a diferent exit message, more cases need to be added.
-- Returns true if the stream ended because of end of file.
-- Returns false if the stream ended because the user closed the stream.
-- Returns nothing an unknown message was read
streamFinishedSuccessfully :: String -> Maybe Bool
streamFinishedSuccessfully "Exiting... (End of file)" = Just True
streamFinishedSuccessfully "Exiting... (Quit)" = Just False
streamFinishedSuccessfully _ = Nothing

streamResult :: String -> String
streamResult stdout
  | result == Just True = "Finished successfully"
  | result == Just False = "Closed the player"
  | otherwise = "WTF did just happen? livestreamer exited with: " ++ stdout
  where result = streamFinishedSuccessfully stdout
