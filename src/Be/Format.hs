{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Format
  ( Options (..)
  , parser
  , run
  )
where

import Be.Tasks
import Beget.Build
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LByteString
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import Database.SQLite.Simple qualified as SQLite
import Options.Applicative qualified as Options
import Prelude hiding (readFile, stderr, stdin, stdout)
import System.Environment.XDG.BaseDir qualified as XDG
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.Typed qualified as Process
import UnliftIO.Async qualified as Async
import UnliftIO.IO.File qualified as IO

data Options = Options
  { command :: Maybe Command
  }

data Command
  = Haskell HaskellOptions
  | Nix NixOptions

data HaskellOptions = HaskellOptions
  { input :: Maybe Input
  }

data NixOptions = NixOptions
  { input :: Maybe Input
  }

data Input
  = Paths (NonEmpty FilePath)
  | Stdin

parser :: Options.ParserInfo Options
parser = Options.info parse info
  where
  info = mconcat
    [ Options.progDesc "Format code"
    ]

  parse = do
    let parsePaths = do
          fmap (Paths . fromList) . some . Options.strArgument . mconcat $
            [ Options.metavar "PATHS"
            , Options.help "Only format specific paths"
            ]

    let parseStdin = do
          Options.flag' Stdin . mconcat $
            [ Options.long "stdin"
            , Options.help "Format code piped to `stdin`"
            ]

    let parseHaskell = do
          input <- optional (parsePaths <|> parseStdin)
          pure $ Haskell HaskellOptions{ input }

    let parseNix = do
          input <- optional (parsePaths <|> parseStdin)
          pure $ Nix NixOptions{ input }

    command <-
      optional . Options.hsubparser . mconcat $
        [ Options.command "haskell" $ Options.info parseHaskell . mconcat $
            [ Options.progDesc "Format Haskell code"
            ]
        , Options.command "nix" $ Options.info parseNix . mconcat $
            [ Options.progDesc "Format Nix code"
            ]
        ]

    pure Options{ command }

fourmoluConfig :: Build FilePath
fourmoluConfig = do
  root <- realize GitRoot
  pure (root </> "fourmolu.yaml")

registerTaskVolatile 'fourmoluConfig

fourmoluExtensions :: Build (Set String)
fourmoluExtensions = do
  root <- realize GitRoot
  let path = root </> "hpack-common" </> "default-extensions.yaml"
  bytes <- readFile path
  case Yaml.decodeEither' (toStrict bytes) of
    Left err -> error $ "Failed to parse " <> toText path <> ": " <> show err
    Right val ->
      case Yaml.parseMaybe parseExtensions val of
        Nothing -> error $ "Missing `default-extensions` key in " <> toText path
        Just exts -> pure (Set.fromList exts)
  where
  parseExtensions :: Yaml.Value -> Yaml.Parser [String]
  parseExtensions = Yaml.withObject "extensions" \o ->
    o Yaml..: "default-extensions"

registerTaskVolatile 'fourmoluExtensions

fourmolu :: FilePath -> LByteString -> Build LByteString
fourmolu path bytes = do
  binary <- realize Which "fourmolu"
  config <- realize FourmoluConfig
  extensions <- realize FourmoluExtensions
  let args =
        [ "--config=" <> config
        , "--no-cabal"
        , "--stdin-input-file=" <> path
        , "--mode=stdout"
        , "--source-type=module"
        , "--unsafe"
        , "--quiet"
        ] <> map ("--ghc-opt=-X" <>) (Set.toList extensions)
  (exitCode, stdout, stderr) <- Process.readProcess $
      sandboxConfig fourmoluProfile binary args
    & Process.setStdin (Process.byteStringInput bytes)
  case exitCode of
    ExitFailure code -> error . unlines $
      [ "`fourmolu` exited with code " <> show code <> ":"
      , decodeUtf8 stderr
      ]
    ExitSuccess -> pure stdout

registerTask 'fourmolu

formatHaskellInPlace :: FilePath -> Build Bool
formatHaskellInPlace path = do
  unformattedBytes <- readFile path
  formattedBytes <- realize Fourmolu path unformattedBytes
  if unformattedBytes == formattedBytes then
    pure False
  else do
    IO.writeBinaryFileAtomic path (toStrict formattedBytes)
    pure True

registerTaskVolatile 'formatHaskellInPlace

nixfmt :: FilePath -> LByteString -> Build LByteString
nixfmt path bytes = do
  binary <- realize Which "nixfmt"
  let args = ["--filename=" <> path, "-"]
  (exitCode, stdout, stderr) <- Process.readProcess $
      sandboxConfig nixfmtProfile binary args
    & Process.setStdin (Process.byteStringInput bytes)
  case exitCode of
    ExitFailure code -> error . unlines $
      [ "`nixfmt` exited with code " <> show code <> ":"
      , decodeUtf8 stderr
      ]
    ExitSuccess -> pure stdout

registerTask 'nixfmt

formatNixInPlace :: FilePath -> Build Bool
formatNixInPlace path = do
  unformattedBytes <- readFile path
  formattedBytes <- realize Nixfmt path unformattedBytes
  if unformattedBytes == formattedBytes then
    pure False
  else do
    IO.writeBinaryFileAtomic path (toStrict formattedBytes)
    pure True

registerTaskVolatile 'formatNixInPlace

runHaskell :: HaskellOptions -> Build ()
runHaskell options = do
  case options.input of
    Just Stdin -> do
      input <- liftIO ByteString.getContents
      output <- realize Fourmolu "/dev/stdin" (toLazy input)
      liftIO $ LByteString.putStr output
    Nothing -> do
      paths <- realize ChangedHaskellFiles
      Async.forConcurrently_ paths \path ->
        formatHaskellInPlace path
    Just (Paths paths) -> do
      Async.forConcurrently_ paths \path -> do
        formatHaskellInPlace path

runNix :: NixOptions -> Build ()
runNix options = do
  case options.input of
    Just Stdin -> do
      input <- liftIO ByteString.getContents
      output <- realize Nixfmt "/dev/stdin" (toLazy input)
      liftIO $ LByteString.putStr output
    Nothing -> do
      paths <- realize ChangedNixFiles
      Async.forConcurrently_ paths \path ->
        formatNixInPlace path
    Just (Paths paths) -> do
      Async.forConcurrently_ paths \path ->
        formatNixInPlace path

run :: Options -> IO ()
run options = do
  sqlitePath <- XDG.getUserCacheFile "be2" "cache.sqlite"
  SQLite.withConnection sqlitePath \connection -> do
    runBuild connection do
      case options.command of
        Just (Haskell haskellOptions) -> runHaskell haskellOptions
        Just (Nix nixOptions) -> runNix nixOptions
        Nothing -> do
          Async.concurrently_
            (runHaskell HaskellOptions{ input = Nothing })
            (runNix NixOptions{ input = Nothing })
