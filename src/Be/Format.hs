{-# LANGUAGE ApplicativeDo #-}

module Be.Format
  ( Options (..)
  , parserInfo
  , run
  )
where

import Options.Applicative qualified as Options
import Prelude hiding (stdin)

data Options = Options
  { command :: Maybe Command
  }

data Command
  = Haskell HaskellOptions
  | Nix NixOptions

data HaskellOptions = HaskellOptions
  { paths :: [FilePath]
  , stdin :: Bool
  }

data NixOptions = NixOptions
  { paths :: [FilePath]
  , stdin :: Bool
  }

parserInfo :: Options.ParserInfo Options
parserInfo = Options.info parse info
  where
  info = mconcat
    [ Options.progDesc "Format code"
    ]

  parse = do
    -- TODO: Make `paths` and `stdin` mutually exclusive

    let parseHaskell = do
          paths <-
            many . Options.strArgument . mconcat $
              [ Options.metavar "PATHS"
              , Options.help "Only format specific paths"
              ]
          stdin <-
            Options.switch . mconcat $
              [ Options.long "stdin"
              , Options.help "Format code piped to `stdin`"
              ]
          pure $ Haskell HaskellOptions{ paths, stdin }

    let parseNix = do
          paths <-
            many . Options.strArgument . mconcat $
              [ Options.metavar "PATHS"
              , Options.help "Only format specific paths"
              ]
          stdin <-
            Options.switch . mconcat $
              [ Options.long "stdin"
              , Options.help "Format code piped to `stdin`"
              ]
          pure $ Nix NixOptions{ paths, stdin }

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
run _options = pure ()
