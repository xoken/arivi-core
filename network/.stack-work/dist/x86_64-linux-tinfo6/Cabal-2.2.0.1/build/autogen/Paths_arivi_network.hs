{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_arivi_network (
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

bindir     = "/home/nithin/work/arivi-core/.stack-work/install/x86_64-linux-tinfo6/efba7c2032d69b4a850cbd1468ea9edd2c6f1997b30d4726ab53e14092a25b62/8.4.4/bin"
libdir     = "/home/nithin/work/arivi-core/.stack-work/install/x86_64-linux-tinfo6/efba7c2032d69b4a850cbd1468ea9edd2c6f1997b30d4726ab53e14092a25b62/8.4.4/lib/x86_64-linux-ghc-8.4.4/arivi-network-0.1.0.0-DBy77fM6D2pHFIbhRQnmv"
dynlibdir  = "/home/nithin/work/arivi-core/.stack-work/install/x86_64-linux-tinfo6/efba7c2032d69b4a850cbd1468ea9edd2c6f1997b30d4726ab53e14092a25b62/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/nithin/work/arivi-core/.stack-work/install/x86_64-linux-tinfo6/efba7c2032d69b4a850cbd1468ea9edd2c6f1997b30d4726ab53e14092a25b62/8.4.4/share/x86_64-linux-ghc-8.4.4/arivi-network-0.1.0.0"
libexecdir = "/home/nithin/work/arivi-core/.stack-work/install/x86_64-linux-tinfo6/efba7c2032d69b4a850cbd1468ea9edd2c6f1997b30d4726ab53e14092a25b62/8.4.4/libexec/x86_64-linux-ghc-8.4.4/arivi-network-0.1.0.0"
sysconfdir = "/home/nithin/work/arivi-core/.stack-work/install/x86_64-linux-tinfo6/efba7c2032d69b4a850cbd1468ea9edd2c6f1997b30d4726ab53e14092a25b62/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "arivi_network_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arivi_network_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "arivi_network_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "arivi_network_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arivi_network_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "arivi_network_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
