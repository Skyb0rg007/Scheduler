{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Scheduler (
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

bindir     = "C:\\Users\\skyle\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\skyle\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\Scheduler-0.1.0.0-9ZzrE4Z7n2dLfVkVm9zXbO"
dynlibdir  = "C:\\Users\\skyle\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\skyle\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.2\\Scheduler-0.1.0.0"
libexecdir = "C:\\Users\\skyle\\AppData\\Roaming\\cabal\\Scheduler-0.1.0.0-9ZzrE4Z7n2dLfVkVm9zXbO"
sysconfdir = "C:\\Users\\skyle\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Scheduler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Scheduler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Scheduler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Scheduler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Scheduler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Scheduler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
