{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Lint
  ( Options (..)
  , parser
  , run
  )
where

import Be.Tasks
import Beget.Build
import Codec.Serialise (Serialise)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString

import Database.SQLite.Simple qualified as SQLite
import Options.Applicative qualified as Options
import Prelude hiding (readFile, stderr, stdin, stdout)
import System.Directory qualified as Directory
import System.Environment.XDG.BaseDir qualified as XDG
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeExtension)
import System.Process.Typed qualified as Process
import UnliftIO.Async qualified as Async

data Options = Options
  { command :: Maybe Command
  }

data Command
  = Haskell HaskellOptions

data HaskellOptions = HaskellOptions
  { input :: Maybe Input
  }

data Input
  = Paths (NonEmpty FilePath)
  | Stdin

parser :: Options.ParserInfo Options
parser = Options.info parse info
  where
  info = mconcat
    [ Options.progDesc "Lint code"
    ]

  parse = do
    let parsePaths = do
          fmap (Paths . fromList) . some . Options.strArgument . mconcat $
            [ Options.metavar "PATHS"
            , Options.help "Only lint specific paths"
            ]

    let parseStdin = do
          Options.flag' Stdin . mconcat $
            [ Options.long "stdin"
            , Options.help "Lint code piped to `stdin`"
            ]

    let parseHaskell = do
          input <- optional (parsePaths <|> parseStdin)
          pure $ Haskell HaskellOptions{ input }

    command <-
      optional . Options.hsubparser . mconcat $
        [ Options.command "haskell" $ Options.info parseHaskell . mconcat $
            [ Options.progDesc "Lint Haskell code"
            ]
        ]

    pure Options{ command }

-- HLint hint data types

data HlintSeverity
  = Ignore
  | Suggestion
  | Warning
  | HlintError
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, Hashable)

instance Aeson.FromJSON HlintSeverity where
  parseJSON = Aeson.withText "HlintSeverity" \case
    "Ignore" -> pure Ignore
    "Suggestion" -> pure Suggestion
    "Warning" -> pure Warning
    "Error" -> pure HlintError
    other -> fail $ "Unknown hlint severity: " <> toString other

formatSeverity :: HlintSeverity -> Text
formatSeverity = \case
  Ignore -> "Ignore"
  Suggestion -> "Suggestion"
  Warning -> "Warning"
  HlintError -> "Error"

data HlintHint = HlintHint
  { severity :: HlintSeverity
  , hint :: Text
  , file :: FilePath
  , startLine :: Int
  , startColumn :: Int
  , from :: Text
  , to :: Maybe Text
  , note :: [Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, Hashable)

instance Aeson.FromJSON HlintHint where
  parseJSON = Aeson.withObject "HlintHint" \o -> do
    severity <- o .: "severity"
    hint <- o .: "hint"
    file <- o .: "file"
    startLine <- o .: "startLine"
    startColumn <- o .: "startColumn"
    from <- o .: "from"
    to <- o .: "to"
    note <- o .: "note"
    pure HlintHint{..}

formatHint :: HlintHint -> Text
formatHint h = unlines $
  [ toText h.file <> ":" <> show h.startLine <> ":" <> show h.startColumn <> ": " <> formatSeverity h.severity <> ": " <> h.hint
  , "Found:"
  , "  " <> h.from
  ] <>
  maybe [] (\t -> ["Perhaps:", "  " <> t]) h.to <>
  map (\n -> "Note: " <> n) h.note

-- HLint config discovery
hlintConfigs :: Build [FilePath]
hlintConfigs = do
  root <- realize GitRoot
  let hlintYaml = root </> ".hlint.yaml"
  hlintYamlExists <- liftIO $ Directory.doesFileExist hlintYaml
  let baseConfigs = [hlintYaml | hlintYamlExists]
  let rulesDir = root </> "hlint-rules"
  rulesDirExists <- liftIO $ Directory.doesDirectoryExist rulesDir
  ruleConfigs <- if rulesDirExists then do
    entries <- liftIO $ Directory.listDirectory rulesDir
    let yamlFiles = filter (\f -> takeExtension f == ".yaml") entries
    pure (map (rulesDir </>) yamlFiles)
  else
    pure []
  pure (baseConfigs <> ruleConfigs)

registerTaskVolatile 'hlintConfigs

-- HLint invocation: runs hlint on stdin with JSON output
hlint :: FilePath -> LByteString -> Build [HlintHint]
hlint path bytes = do
  binary <- realize Which "hlint"
  configs <- realize HlintConfigs
  let configArgs = map ("--hint=" <>) configs
  let args = ["--json", "--no-exit-code", "-"] <> configArgs
  (exitCode, stdout, stderr) <- Process.readProcess $
      sandboxConfig hlintProfile binary args
    & Process.setStdin (Process.byteStringInput bytes)
  case exitCode of
    ExitFailure code -> error . unlines $
      [ "`hlint` exited with code " <> show code <> ":"
      , decodeUtf8 stderr
      ]
    ExitSuccess ->
      case Aeson.eitherDecodeStrict' (toStrict stdout) of
        Left err -> error $ "Failed to parse hlint output: " <> toText err
        Right hints -> pure (map (\h -> h{ file = path }) hints)

registerTask 'hlint

lintHaskellFile :: FilePath -> Build [HlintHint]
lintHaskellFile path = do
  bytes <- readFile path
  realize Hlint path bytes

registerTaskVolatile 'lintHaskellFile

runHaskell :: HaskellOptions -> Build ()
runHaskell options = do
  case options.input of
    Just Stdin -> do
      input <- liftIO ByteString.getContents
      hints <- realize Hlint "/dev/stdin" (toLazy input)
      liftIO $ mapM_ (putText . formatHint) hints
    Nothing -> do
      paths <- realize ChangedHaskellFiles
      allHints <- Async.forConcurrently paths \path ->
        lintHaskellFile path
      liftIO $ mapM_ (putText . formatHint) (concat allHints)
    Just (Paths paths) -> do
      allHints <- Async.forConcurrently (toList paths) \path ->
        lintHaskellFile path
      liftIO $ mapM_ (putText . formatHint) (concat allHints)

run :: Options -> IO ()
run options = do
  sqlitePath <- XDG.getUserCacheFile "be2" "cache.sqlite"
  SQLite.withConnection sqlitePath \connection -> do
    runBuild connection do
      case options.command of
        Just (Haskell haskellOptions) -> runHaskell haskellOptions
        Nothing -> runHaskell HaskellOptions{ input = Nothing }
