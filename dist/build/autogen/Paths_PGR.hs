module Paths_PGR (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/oriaj/.cabal/bin"
libdir     = "/home/oriaj/.cabal/lib/x86_64-linux-ghc-7.8.3/PGR-0.0.0"
datadir    = "/home/oriaj/.cabal/share/x86_64-linux-ghc-7.8.3/PGR-0.0.0"
libexecdir = "/home/oriaj/.cabal/libexec"
sysconfdir = "/home/oriaj/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PGR_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PGR_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PGR_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PGR_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PGR_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
