{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_funky_hs (
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

bindir     = "C:\\Code\\Haskell\\funky-hs\\.stack-work\\install\\58752683\\bin"
libdir     = "C:\\Code\\Haskell\\funky-hs\\.stack-work\\install\\58752683\\lib\\x86_64-windows-ghc-8.0.2\\funky-hs-0.1.0.0-IF0pP31mnov4Y2vVMpNn76"
dynlibdir  = "C:\\Code\\Haskell\\funky-hs\\.stack-work\\install\\58752683\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Code\\Haskell\\funky-hs\\.stack-work\\install\\58752683\\share\\x86_64-windows-ghc-8.0.2\\funky-hs-0.1.0.0"
libexecdir = "C:\\Code\\Haskell\\funky-hs\\.stack-work\\install\\58752683\\libexec"
sysconfdir = "C:\\Code\\Haskell\\funky-hs\\.stack-work\\install\\58752683\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "funky_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "funky_hs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "funky_hs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "funky_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "funky_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "funky_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
