module Main where

import Prelude
import Control.Monad.Except
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe

import System.Environment

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



hot, main :: IO ()
hot@main = ioExcept $ do

  ps <- liftIO getArgs
  p <- case ps of
    p : _ -> return p
    _ -> throwError "no path specified"

  let o = defaultDecodeOptions {decodeOptionsTarget = p }
  r :: DecodeResult <- embed $ readPackageConfig o

  let
    p = decodeResultPackage r
    lib = maybe [] pure $ packageLibrary p -- :: Maybe (Section Library)
    ilib = M.toList $ packageInternalLibraries p -- :: Map String (Section Library)
    es = M.toList $ packageExecutables p -- :: Map String (Section Executable)
    ts = M.toList $ packageTests p -- :: Map String (Section Executable)
    bs = M.toList $ packageBenchmarks p -- :: Map String (Section Executable)

    g a = sectionDefaultExtensions . snd $ a
    f a = map g a

    all = map sectionDefaultExtensions lib <> f ilib <> f es <> f ts <> f bs

  liftIO $ print all
  liftIO $ putStrLn "end"
