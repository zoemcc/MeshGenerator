module Paths_MeshGenerator (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/zoe/dev/haskell/MeshGenerator/.cabal-sandbox/bin"
libdir     = "/Users/zoe/dev/haskell/MeshGenerator/.cabal-sandbox/lib/x86_64-osx-ghc-7.6.3/MeshGenerator-0.1.0.0"
datadir    = "/Users/zoe/dev/haskell/MeshGenerator/.cabal-sandbox/share/x86_64-osx-ghc-7.6.3/MeshGenerator-0.1.0.0"
libexecdir = "/Users/zoe/dev/haskell/MeshGenerator/.cabal-sandbox/libexec"
sysconfdir = "/Users/zoe/dev/haskell/MeshGenerator/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MeshGenerator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MeshGenerator_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MeshGenerator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MeshGenerator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MeshGenerator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
