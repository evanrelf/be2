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
import Options.Applicative qualified as Options
import Prelude hiding (stdin)
import UnliftIO.Async qualified as Async

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

run :: Options -> IO ()
run options = do
  case options.command of
    Just (Haskell haskellOptions) -> runHaskell haskellOptions
    Just (Nix nixOptions) -> runNix nixOptions
    Nothing -> do
      Async.concurrently_
        (runHaskell HaskellOptions{ input = Nothing })
        (runNix NixOptions{ input = Nothing })

runHaskell :: HaskellOptions -> IO ()
runHaskell _options = do
  pure ()

runNix :: NixOptions -> IO ()
runNix _options = do
  pure ()

gitRoot :: Build FilePath
gitRoot = do
  undefined

registerTaskWith 'gitRoot defaultTaskOptions{ volatile = True }

-- TODO: Resolve all symlinks (i.e. `realpath`).
which :: Text -> Build FilePath
which name = do
  undefined

registerTaskWith 'which defaultTaskOptions{ volatile = True }

-- TODO: Place copy in consistent location (e.g. `~/.cache/be2/fourmolu-<hash>.yaml`).
-- TODO: Register temporary file as resource to be cleaned up at end of program.
fourmoluConfig :: Build FilePath
fourmoluConfig = do
  undefined

registerTaskWith 'fourmoluConfig defaultTaskOptions{ volatile = True }

fourmoluExtensions :: Build (Set Text)
fourmoluExtensions = do
  undefined

registerTaskWith 'fourmoluExtensions defaultTaskOptions{ volatile = True }

fourmolu :: FilePath -> ByteString -> Build ByteString
fourmolu path bytes = do
  undefined

registerTask 'fourmolu
