module Paths_ascetic (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Users\\al\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\al\\AppData\\Roaming\\cabal\\ascetic-0.0.0.1\\ghc-7.4.2"
datadir    = "C:\\Users\\al\\AppData\\Roaming\\cabal\\ascetic-0.0.0.1"
libexecdir = "C:\\Users\\al\\AppData\\Roaming\\cabal\\ascetic-0.0.0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ascetic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ascetic_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ascetic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ascetic_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
