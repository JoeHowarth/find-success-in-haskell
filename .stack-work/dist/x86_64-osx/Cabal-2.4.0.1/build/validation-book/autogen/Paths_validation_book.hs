{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_validation_book (
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

bindir     = "/Users/jh/Desktop/projects/haskell-proj/validation-book/.stack-work/install/x86_64-osx/74ebc9f67f1fd85e09949c44943b0b0cb6d70403c9756a75fc8bb985b8d1a20d/8.6.5/bin"
libdir     = "/Users/jh/Desktop/projects/haskell-proj/validation-book/.stack-work/install/x86_64-osx/74ebc9f67f1fd85e09949c44943b0b0cb6d70403c9756a75fc8bb985b8d1a20d/8.6.5/lib/x86_64-osx-ghc-8.6.5/validation-book-0.1.0.0-9PNh3gs6euS9kJHkMLn6Rx-validation-book"
dynlibdir  = "/Users/jh/Desktop/projects/haskell-proj/validation-book/.stack-work/install/x86_64-osx/74ebc9f67f1fd85e09949c44943b0b0cb6d70403c9756a75fc8bb985b8d1a20d/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/jh/Desktop/projects/haskell-proj/validation-book/.stack-work/install/x86_64-osx/74ebc9f67f1fd85e09949c44943b0b0cb6d70403c9756a75fc8bb985b8d1a20d/8.6.5/share/x86_64-osx-ghc-8.6.5/validation-book-0.1.0.0"
libexecdir = "/Users/jh/Desktop/projects/haskell-proj/validation-book/.stack-work/install/x86_64-osx/74ebc9f67f1fd85e09949c44943b0b0cb6d70403c9756a75fc8bb985b8d1a20d/8.6.5/libexec/x86_64-osx-ghc-8.6.5/validation-book-0.1.0.0"
sysconfdir = "/Users/jh/Desktop/projects/haskell-proj/validation-book/.stack-work/install/x86_64-osx/74ebc9f67f1fd85e09949c44943b0b0cb6d70403c9756a75fc8bb985b8d1a20d/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "validation_book_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "validation_book_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "validation_book_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "validation_book_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "validation_book_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "validation_book_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
