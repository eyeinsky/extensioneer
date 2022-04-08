module Main where

import Prelude
import Control.Monad.Except
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.List qualified as L
import Data.Kind
import Debug.Trace
import Text.Printf

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

type C :: (Type -> Type) -> Constraint
type C m = (Monad m, MonadError String m, MonadIO m)

hot, main :: IO ()
hot@main = ioExcept $ do
  filePaths <- argsToFiles =<< liftIO getArgs
  printSummaryTable filePaths

-- * Extension table for multiple packages

argsToFiles :: C m => [String] -> m [FilePath]
argsToFiles args = do
  bools <- mapM (liftIO . doesFileExist) args
  return $ map snd $ filter fst $ zip bools args

printSummaryTable :: C m => [FilePath] -> m ()
printSummaryTable paths = do
  let f2n = zip paths [0..]
  pairs <- getFilesExtensions paths
  liftIO $ do
    forM_ f2n $ \(p, n) -> do
      putStrLn $ "# " <> show n <> " - " <> p
    putStrLn ""

    let n = length paths

    forM_ (merge pairs) $ \(ext, ps) -> do
      let ns = catMaybes $ L.sort $ map (flip lookup f2n) ps
          strs = map f $ mbList 0 ns
            where f = \case
                    Just n -> show n
                    _ -> " "
      printf "- %-28s # %s\n" ext $ L.intercalate " " strs

mbList :: Int -> [Int] -> [Maybe Int]
mbList n yss = case yss of
  (y : ys) -> if n == y
    then Just n : next ys
    else Nothing : next yss
    where next = mbList (n + 1)
  _ -> []

-- * Pure

type FileExts = [(FilePath, [String])]
type ExtFs = [(String, [FilePath])]

merge :: FileExts -> ExtFs
merge = pass
  where
    pass :: FileExts -> ExtFs
    pass xs = case xs of
      _ : _
        | heads@(_:_) <- nextExts xs -> let
          ext = head $ L.sort heads
          files = map fst $ filter f xs
            where
              f ys = case ys of
                (p, y : _) -> y == ext
                _ -> False
          xs' = catMaybes $ map f xs
            where f = \case
                    a@(p, y : ys) -> Just $ if y == ext then (p, ys) else a
                    _ -> Nothing
          in (ext, files) : pass xs'
      _ -> []

    nextExts :: FileExts -> [String]
    nextExts xs = catMaybes $ map (listToMaybe . snd) xs

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

getFileExtensions :: C m => FilePath -> m [String]
getFileExtensions path = do
  let opts = defaultDecodeOptions {decodeOptionsTarget = path }
  result :: DecodeResult <- embed $ readPackageConfig opts
  return $ getAllExtensions result

getFilesExtensions :: C m => [FilePath] -> m FileExts
getFilesExtensions fs = traverse (\p -> (p,) <$> getFileExtensions p) fs

withDir :: C m => FilePath -> m ()
withDir p = do
  liftIO $ putStrLn $ "directory argument: " <> p
