module Main where

import Prelude
import Control.Monad.Except
import Control.Arrow
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.List qualified as L
import Data.Either qualified as E
import Data.Kind
import Debug.Trace
import Text.Printf

import System.Environment
import System.Directory

-- | Hpack
import Hpack.Yaml as Hpack
import Hpack as Hpack
import Hpack.Config as Hpack

-- | Cabal
import Distribution.Types.BuildInfo as Cabal
import Language.Haskell.Extension as Cabal
import Distribution.Simple as Cabal
-- import Distribution.Simple.PackageDescription
import Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Verbosity as Cabal


import PredefinedExtensionSets

data Args = Args FilePath

-- * Base monad

ioExcept :: ExceptT String IO () -> IO ()
ioExcept e = runExceptT e >>= \case
  Left e -> putStrLn $ "error: " <> e
  Right r -> pure ()

embed :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
embed action = liftIO action >>= either throwError pure

type C :: (Type -> Type) -> Constraint
type C m = (Monad m, MonadError String m, MonadIO m)

-- *

data PackageFilePath
  = Cabal FilePath
  | Hpack FilePath
  deriving stock Eq

path :: PackageFilePath -> FilePath
path = \case
  Cabal p -> p
  Hpack p -> p

hot, main :: IO ()
hot@main = ioExcept $ do
  packageFilePaths <- argsToFiles =<< liftIO getArgs
  printSummaryTable packageFilePaths

-- * Extension table for multiple packages

argsToFiles :: C m => [String] -> m [PackageFilePath]
argsToFiles args = do
  bools <- mapM (liftIO . doesFileExist) args
  let zip' = zip bools args
      (existing, nonExisting) = (map snd *** map snd) $ L.partition fst zip'
      (hpackFiles, existing') = L.partition (".yaml" `L.isSuffixOf`) existing
      (cabalFiles, existing'') = L.partition (".cabal" `L.isSuffixOf`) existing'

      f path | ".yaml" `L.isSuffixOf` path = Right (Hpack path)
             | ".cabal" `L.isSuffixOf` path = Right (Cabal path)
             | otherwise = Left path

      (unknown, hpackOrCabal) = E.partitionEithers $ map f args

  when (not $ null nonExisting) $ throwError
    $ "The following paths do not exist: \n"
    <> unlines (map ("- " <>) nonExisting)
  when (not $ null unknown) $ throwError
    $ "Don't recognize the file extension for following files: \n"
    <> unlines (map ("- " <>) existing'')

  return hpackOrCabal

printSummaryTable :: C m => [PackageFilePath] -> m ()
printSummaryTable paths = do
  let f2n = zip paths [0..]
  pairs <- getFilesExtensions paths
  liftIO $ do
    forM_ f2n $ \(p, n) -> do
      putStrLn $ "# " <> show n <> " - " <> path p
    putStrLn ""

    forM_ (merge pairs) $ \(ext, ps) -> do
      let ns = catMaybes $ L.sort $ map (flip lookup f2n) ps
          strs = map f $ boolList 0 ns
            where
              f (n, b) = let s = show n
                in if b then s else replicate (length s) ' '
      printf "- %-28s # %s\n" ext $ L.intercalate " " strs

boolList :: Int -> [Int] -> [(Int, Bool)]
boolList n yss = case yss of
  (y : ys) -> let b = n == y
    in (n, b) : boolList (n + 1) (if b then ys else yss)
  _ -> []

-- * Pure

type FileExts = [(PackageFilePath, [String])]
type ExtFs = [(String, [PackageFilePath])]

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

getFilesExtensions :: C m => [PackageFilePath] -> m FileExts
getFilesExtensions fs = flip traverse fs $ \pfp -> case pfp of
  Hpack p -> (pfp,) <$> hpackGetExtensions p
  Cabal p -> (pfp,) <$> cabalGetExtensions p

-- * Hpack

hpackGetAllExtensions :: DecodeResult -> [String]
hpackGetAllExtensions result =
  let
    pkg = decodeResultPackage result
    lib = maybe [] pure $ packageLibrary pkg :: [Section Hpack.Library]
    ilib = packageInternalLibraries pkg :: M.Map String (Section Hpack.Library)
    es = packageExecutables pkg :: M.Map String (Section Hpack.Executable)
    ts = packageTests pkg :: M.Map String (Section Hpack.Executable)
    bs = packageBenchmarks pkg :: M.Map String (Section Hpack.Executable)

    mapExts a = map (sectionDefaultExtensions . snd) $ M.toList a

    all = map sectionDefaultExtensions lib
      <> mapExts ilib
      <> mapExts es
      <> mapExts ts
      <> mapExts bs

  in order $ concat all

hpackGetExtensions :: C m => FilePath -> m [String]
hpackGetExtensions path = do
  let opts = defaultDecodeOptions {decodeOptionsTarget = path }
  result :: DecodeResult <- embed $ readPackageConfig opts
  return $ hpackGetAllExtensions result

-- * Cabal

-- | Gather all extensions from every corner of the cabal file.
--
-- https://hackage.haskell.org/package/Cabal/docs/Distribution-Types-GenericPackageDescription.html#t:GenericPackageDescription
cabalGetExtensions' :: FilePath -> IO [Extension]
cabalGetExtensions' path = do
  gpd <- liftIO $ readGenericPackageDescription silent path
  let
      libsExts = map (allExtensions . libBuildInfo)
      execsExts = map (allExtensions . buildInfo)

      pd = Cabal.packageDescription gpd
      lib, libs, exts :: [[Extension]]
      lib = maybe [] (pure . allExtensions . libBuildInfo) (library pd)
      libs = libsExts $ subLibraries pd
      exts = execsExts $ executables pd

      condDatas :: forall v c a . CondTree v c a -> [a]
      condDatas a = let
        branches = condTreeComponents a
        trues = condDatas =<< map condBranchIfTrue branches
        falses = condDatas =<< catMaybes (map condBranchIfFalse branches)
        in condTreeData a : (trues <> falses)

      lib_ = libsExts $ maybe [] condDatas (condLibrary gpd)
      libs_ = libsExts $ condDatas =<< map snd (condSubLibraries gpd)
      execs_ = execsExts $ condDatas =<< map snd (condExecutables gpd)

      all = concat $ lib <> libs <> exts <> lib_ <> libs_ <> execs_

  return all

cabalGetExtensions :: C m => FilePath -> m [String]
cabalGetExtensions path =
  fmap order . mapM ext2string =<< liftIO (cabalGetExtensions' path)

ext2string :: C m => Extension -> m String
ext2string = \case
  EnableExtension e -> return $ show e
  DisableExtension e -> return $ "No" <> show e
  UnknownExtension e -> throwError
    $ "ext2string: illegal extension '" <> e <> "'"

-- * Helpers

-- | Deduplicate and sort list
order :: Eq a => Ord a => [a] -> [a]
order = L.sort . L.nub
