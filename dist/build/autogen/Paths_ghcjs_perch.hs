module Paths_ghcjs_perch (
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
version = Version [0,2,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghcjs-0.2.0-ghc7_10_2\\ghcjs-perch-0.2.0.0-6EiBdv3ZxnUFQKWII9QlN1"
datadir    = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\x86_64-windows-ghcjs-0.2.0-ghc7_10_2\\ghcjs-perch-0.2.0.0"
libexecdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\ghcjs-perch-0.2.0.0-6EiBdv3ZxnUFQKWII9QlN1"
sysconfdir = "C:\\Users\\magocoal\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ghcjs_perch_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ghcjs_perch_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ghcjs_perch_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghcjs_perch_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ghcjs_perch_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
