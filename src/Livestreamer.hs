module Livestreamer (
  stream,
  streamFinishedSuccessfully,
  streamResult
) where

import Data.Maybe
import System.IO
import System.Process

-- Starts a new livestreamer process to stream an episode.
-- Receives the episode's crunchyroll URL as argument, and optionally the quality.
-- If quality is Nothing defaults to ultra quality.
--  Returns a String with livestreamer's exit message since exit code is always 0
-- To ensure that the exit message is always printed, starts the process with
-- verbose-player flag.
stream :: String -> Maybe String -> IO String
stream episodeUrl quality = do
    (code, stdout, stderr) <- readProcessWithExitCode "livestreamer"
      ["--verbose-player", episodeUrl, fromMaybe "ultra" quality] ""
    return stdout

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
