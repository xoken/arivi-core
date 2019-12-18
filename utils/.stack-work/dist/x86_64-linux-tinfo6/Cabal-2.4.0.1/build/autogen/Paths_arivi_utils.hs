{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_arivi_utils (
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

bindir     = "/home/nithin/work/xoken-node/.stack-work/install/x86_64-linux-tinfo6/c38f0d199b70aafebc8221545e851f55700fed7328bfa7a7ce677ee8a7a51502/8.6.5/bin"
libdir     = "/home/nithin/work/xoken-node/.stack-work/install/x86_64-linux-tinfo6/c38f0d199b70aafebc8221545e851f55700fed7328bfa7a7ce677ee8a7a51502/8.6.5/lib/x86_64-linux-ghc-8.6.5/arivi-utils-0.1.0.0-Hb7zAlI5NAqFHjFUDz2pYH"
dynlibdir  = "/home/nithin/work/xoken-node/.stack-work/install/x86_64-linux-tinfo6/c38f0d199b70aafebc8221545e851f55700fed7328bfa7a7ce677ee8a7a51502/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/nithin/work/xoken-node/.stack-work/install/x86_64-linux-tinfo6/c38f0d199b70aafebc8221545e851f55700fed7328bfa7a7ce677ee8a7a51502/8.6.5/share/x86_64-linux-ghc-8.6.5/arivi-utils-0.1.0.0"
libexecdir = "/home/nithin/work/xoken-node/.stack-work/install/x86_64-linux-tinfo6/c38f0d199b70aafebc8221545e851f55700fed7328bfa7a7ce677ee8a7a51502/8.6.5/libexec/x86_64-linux-ghc-8.6.5/arivi-utils-0.1.0.0"
sysconfdir = "/home/nithin/work/xoken-node/.stack-work/install/x86_64-linux-tinfo6/c38f0d199b70aafebc8221545e851f55700fed7328bfa7a7ce677ee8a7a51502/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arivi_utils_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arivi_utils_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "arivi_utils_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "arivi_utils_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arivi_utils_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arivi_utils_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
