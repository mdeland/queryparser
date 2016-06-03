module Paths_qp (
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

bindir     = "/Users/mdeland/src/qp/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/bin"
libdir     = "/Users/mdeland/src/qp/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/qp-0.1.0.0-C3kc01pv30e5vZzSoHmRZh"
datadir    = "/Users/mdeland/src/qp/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/share/x86_64-osx-ghc-7.10.3/qp-0.1.0.0"
libexecdir = "/Users/mdeland/src/qp/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/libexec"
sysconfdir = "/Users/mdeland/src/qp/.stack-work/install/x86_64-osx/lts-6.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "qp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "qp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "qp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "qp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "qp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
