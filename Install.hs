#!/usr/bin/env stack
{- stack
    --resolver lts-7.7
    --install-ghc
    runghc
    --package directory
    --package process
 -}

import System.Directory
import System.Process

catalogSrcDir = "src/catalog/"
templateScript = "src/mpv_template_script.lua"

main :: IO ()
main = do
  userDir <- getHomeDirectory
  let appDir = userDir ++ "/.local/share/crunchy/"
  let catalogDir = appDir ++ "catalog/"
  let storageDir = appDir ++ "db/"

  createDirectoryIfMissing True storageDir
  createProcess (proc "cp" ["-r", catalogSrcDir, catalogDir])
  createProcess (proc "cp" [templateScript, appDir])
  return ()
