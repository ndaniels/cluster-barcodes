{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cluster_barcodes (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/noah/.cabal/bin"
libdir     = "/Users/noah/.cabal/lib/x86_64-osx-ghc-8.6.3/cluster-barcodes-0.1-inplace"
dynlibdir  = "/Users/noah/.cabal/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/noah/.cabal/share/x86_64-osx-ghc-8.6.3/cluster-barcodes-0.1"
libexecdir = "/Users/noah/.cabal/libexec/x86_64-osx-ghc-8.6.3/cluster-barcodes-0.1"
sysconfdir = "/Users/noah/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cluster_barcodes_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cluster_barcodes_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cluster_barcodes_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cluster_barcodes_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cluster_barcodes_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cluster_barcodes_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
