module Paths_engines (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/alpha44/.cabal/bin"
libdir     = "/home/alpha44/.cabal/lib/i386-linux-ghc-7.10.3/engines-0.1.0.0-H6WFFBtiBm05rNdznbD4ct"
datadir    = "/home/alpha44/.cabal/share/i386-linux-ghc-7.10.3/engines-0.1.0.0"
libexecdir = "/home/alpha44/.cabal/libexec"
sysconfdir = "/home/alpha44/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "engines_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "engines_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "engines_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "engines_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "engines_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
