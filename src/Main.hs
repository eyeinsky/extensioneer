module Main where

import Prelude
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.List qualified as L
import Data.Either qualified as E
import Data.Kind
import Control.Monad.Except
import Control.Arrow

import Text.Printf
import System.Environment
import System.Directory
import Options.Applicative as Opts

-- | Hpack
import Hpack.Yaml as Hpack
import Hpack as Hpack
import Hpack.Config as Hpack

-- | Cabal
import Distribution.Types.BuildInfo as Cabal
import Language.Haskell.Extension as Cabal
import Distribution.Simple as Cabal hiding (Args)
import Distribution.PackageDescription as Cabal
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Verbosity as Cabal

import PredefinedExtensionSets


-- * Types

data PackageFilePath
  = Cabal FilePath
  | Hpack FilePath
  deriving stock Eq

path :: PackageFilePath -> FilePath
path = \case
  Cabal p -> p
  Hpack p -> p

type FileExts = (PackageFilePath, [String])
type ExtFs = [(String, [String])]
type LabelExts = (String, [String])

-- * CLI arguments

data Args = Args
  { ghc2021     :: Bool
  , haskell2010 :: Bool
  , haskell98   :: Bool
  , paths       :: [FilePath]
  }

args :: Opts.Parser Args
args = Args
  <$> switch (long "ghc2021" <> help (allOf "GHC2021"))
  <*> switch (long "haskell2010" <> help (allOf "Haskell2010"))
  <*> switch (long "haskell98" <> help (allOf "Haskell98"))
  <*> some (argument str (metavar "paths to cabal or hpack files"))
  where
    allOf title = "Include all extensions from " <> title

opts :: ParserInfo Args
opts = info (args <**> helper)
  ( fullDesc
  <> header "extensioneer - Inspect extensions in cabal and hpack files" )

-- * Main

main :: IO ()
main = ioExcept $ do

  args <- liftIO $ execParser opts
  packageFilePaths <- resolvePaths $ paths args
  fileExtss :: [FileExts] <- getFilesExtensions packageFilePaths

  let
    labels = map path packageFilePaths :: [String]
    labels' =
        prependIf (ghc2021 args) "GHC2021"
      $ prependIf (haskell2010 args) "Haskell2010"
      $ prependIf (haskell98 args) "Haskell98"
      $ labels

    index = zip labels' [0..]

    labelExts = map (first path) fileExtss :: [LabelExts]
    labelExts' =
        prependIf (ghc2021 args) ("GHC2021", lang_GHC2021)
      $ prependIf (haskell2010 args) ("Haskell2010", lang_Haskell2010)
      $ prependIf (haskell98 args) ("Haskell98", lang_Haskell98)
      $ labelExts

  printSummaryTable index labelExts'

type C :: (Type -> Type) -> Constraint
type C m = (Monad m, MonadError String m, MonadIO m)

ioExcept :: ExceptT String IO () -> IO ()
ioExcept e = runExceptT e >>= \case
  Left e -> putStrLn $ "error: " <> e
  Right r -> pure ()

-- | Check if all paths exist, resolve them to cabal and hpack files
-- by extension
resolvePaths :: C m => [String] -> m [PackageFilePath]
resolvePaths args = do
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

printSummaryTable :: C m => [(String, Int)] -> [LabelExts] -> m ()
printSummaryTable index labelExts = do
  liftIO $ do
    forM_ index $ \(p, n) -> do
      putStrLn $ "# " <> show n <> " - " <> p
    putStrLn ""

    forM_ (merge labelExts) $ \(ext, ps) -> do
      let ns = catMaybes $ L.sort $ map (flip lookup index) ps
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

merge :: [LabelExts] -> ExtFs
merge = pass
  where
    pass :: [LabelExts] -> ExtFs
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

    nextExts :: [LabelExts] -> [String]
    nextExts xs = catMaybes $ map (listToMaybe . snd) xs

getFilesExtensions :: C m => [PackageFilePath] -> m [FileExts]
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
  result :: DecodeResult <- liftIO (readPackageConfig opts)
    >>= either throwError pure
  return $ hpackGetAllExtensions result

-- * Cabal

-- | Gather all extensions from every corner of the cabal file.
--
-- https://hackage.haskell.org/package/Cabal/docs/Distribution-Types-GenericPackageDescription.html#t:GenericPackageDescription
cabalGetExtensionsIO :: FilePath -> IO [Extension]
cabalGetExtensionsIO path = do
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
  fmap order . mapM ext2string =<< liftIO (cabalGetExtensionsIO path)

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

prependIf :: Bool -> a -> [a] -> [a]
prependIf b x xs = if b then x : xs else xs
