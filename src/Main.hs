module Main where

import Prelude
import Control.Monad.Except
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.List qualified as L

import System.Environment
import System.Directory

import Hpack.Yaml
import Hpack
import Hpack.Config

data Args = Args FilePath

ioExcept :: ExceptT String IO () -> IO ()
ioExcept e = runExceptT e >>= \case
  Left e -> putStrLn $ "error: " <> e
  Right _ -> pure ()

embed :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
embed action = liftIO action >>= either throwError pure

type M m a = (MonadError String m, MonadIO m) => m a

hot, main :: IO ()
hot@main = ioExcept $ do

  ps <- liftIO getArgs
  p <- case ps of
    p : _ -> return p
    _ -> throwError "no path specified"

  dir <- liftIO $ doesDirectoryExist p
  if dir
    then withDir p
    else do
      file <- liftIO $ doesFileExist p
      if file
        then do
          exts <- getFileExtensions p
          liftIO $ do
            putStrLn $ "# Extensions used in '" <> p <> "'"
            forM_ exts $ \e -> putStrLn $ "- " <> e
        else throwError $ "'" <> p <> "' is not a file nor a directory"

getAllExtensions :: DecodeResult -> [String]
getAllExtensions result =
  let
    pkg = decodeResultPackage result
    lib = maybe [] pure $ packageLibrary pkg -- :: Maybe (Section Library)
    ilib = M.toList $ packageInternalLibraries pkg -- :: Map String (Section Library)
    es = M.toList $ packageExecutables pkg -- :: Map String (Section Executable)
    ts = M.toList $ packageTests pkg -- :: Map String (Section Executable)
    bs = M.toList $ packageBenchmarks pkg -- :: Map String (Section Executable)

    g a = sectionDefaultExtensions . snd $ a
    f a = map g a

    all = map sectionDefaultExtensions lib <> f ilib <> f es <> f ts <> f bs
    all' = L.sort $ L.nub all
  in concat all'

getFileExtensions :: FilePath -> M m [String]
getFileExtensions path = do
  let opts = defaultDecodeOptions {decodeOptionsTarget = path }
  result :: DecodeResult <- embed $ readPackageConfig opts
  return $ getAllExtensions result



withDir :: FilePath -> M m ()
withDir p = do
  liftIO $ putStrLn $ "directory argument: " <> p
