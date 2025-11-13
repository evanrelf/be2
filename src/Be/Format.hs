{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Be.Format
  ( Options (..)
  , parser
  , run
  )
where

import Be.Core.Build
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LByteString
import Data.Set qualified as Set
import Database.SQLite.Simple qualified as SQLite
import Options.Applicative qualified as Options
import Prelude hiding (readFile, stderr, stdin, stdout)
import System.Environment.XDG.BaseDir qualified as XDG
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.Typed qualified as Process
import UnliftIO.Async qualified as Async
import UnliftIO.IO qualified as IO
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

readFile :: MonadIO m => FilePath -> m LByteString
readFile path = liftIO do
  IO.withBinaryFile path IO.ReadMode \handle ->
    LByteString.hGetContents handle

exec :: MonadIO m => FilePath -> [String] -> m LByteString
exec program args = Process.readProcessStdout_ config
  where config = Process.proc program args

gitRoot :: Build FilePath
gitRoot = do
  pathBytes <- exec "git" ["rev-parse", "--show-toplevel"]
  let path = decodeUtf8 (Char8.strip (toStrict pathBytes))
  pure path

registerTaskVolatile 'gitRoot

which :: String -> Build FilePath
which name = do
  initialPathBytes <- exec "which" [name]
  let initialPath = decodeUtf8 (Char8.strip (toStrict initialPathBytes))
  realPathBytes <- exec "realpath" [initialPath]
  let realPath = decodeUtf8 (Char8.strip (toStrict realPathBytes))
  pure realPath

registerTaskVolatile 'which

-- TODO: Place copy in consistent location (e.g. `~/.cache/be2/fourmolu-<hash>.yaml`).
-- TODO: Register temporary file as resource to be cleaned up at end of program.
fourmoluConfig :: Build FilePath
fourmoluConfig = do
  root <- realize GitRoot
  let path = root </> "fourmolu.yaml"
  pure undefined

registerTaskVolatile 'fourmoluConfig

fourmoluExtensions :: Build (Set String)
fourmoluExtensions = do
  undefined

registerTaskVolatile 'fourmoluExtensions

-- TODO: Canonicalize path.
-- TODO: Sandbox process.
-- TODO: Use process permits.
-- TODO: Use file permits.
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
      Process.proc binary args
    & Process.setEnv []
    & Process.setWorkingDir "/var/empty"
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

runHaskell :: HaskellOptions -> Build ()
runHaskell options = do
  case options.input of
    Just Stdin -> do
      input <- liftIO ByteString.getContents
      output <- realize Fourmolu "/dev/stdin" (toLazy input)
      liftIO $ LByteString.putStr output
    Nothing -> do
      -- TODO: Use changed paths from Git.
      error "not yet implemented"
    Just (Paths paths) -> do
      Async.forConcurrently_ paths \path -> do
        formatHaskellInPlace path

runNix :: NixOptions -> Build ()
runNix _options = do
  pure ()

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
