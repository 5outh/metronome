module Paths_metronome (
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

bindir     = "/Users/Ben/projects/metronome/.stack-work/install/x86_64-osx/nightly-2016-05-16/7.10.3/bin"
libdir     = "/Users/Ben/projects/metronome/.stack-work/install/x86_64-osx/nightly-2016-05-16/7.10.3/lib/x86_64-osx-ghc-7.10.3/metronome-0.1.0.0-3vfruzgzilDAKomPXFYNIJ"
datadir    = "/Users/Ben/projects/metronome/.stack-work/install/x86_64-osx/nightly-2016-05-16/7.10.3/share/x86_64-osx-ghc-7.10.3/metronome-0.1.0.0"
libexecdir = "/Users/Ben/projects/metronome/.stack-work/install/x86_64-osx/nightly-2016-05-16/7.10.3/libexec"
sysconfdir = "/Users/Ben/projects/metronome/.stack-work/install/x86_64-osx/nightly-2016-05-16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "metronome_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "metronome_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "metronome_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "metronome_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "metronome_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
