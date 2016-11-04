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

main :: IO ()
main = do
  userDir <- getHomeDirectory
  let appDir = userDir ++ "/.local/share/crunchy/"
  let catalogDir = appDir ++ "catalog/"
  let storageDir = appDir ++ "db/"

  createDirectoryIfMissing True storageDir
  createProcess (proc "cp" ["-r", catalogSrcDir, catalogDir])
  return ()
