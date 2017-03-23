{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_irc (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/masse/programming/irc/.styx/bin"
libdir     = "/home/masse/programming/irc/.styx/lib/x86_64-linux-ghc-8.0.1/irc-0.1.0.0"
dynlibdir  = "/home/masse/programming/irc/.styx/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/masse/programming/irc/.styx/share/x86_64-linux-ghc-8.0.1/irc-0.1.0.0"
libexecdir = "/home/masse/programming/irc/.styx/libexec"
sysconfdir = "/home/masse/programming/irc/.styx/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "irc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "irc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "irc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "irc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "irc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "irc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
