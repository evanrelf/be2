{-# LANGUAGE ApplicativeDo #-}

module Be.Lint
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

data HaskellOptions = HaskellOptions
  { paths :: [FilePath]
  , stdin :: Bool
  }

parserInfo :: Options.ParserInfo Options
parserInfo = Options.info parse info
  where
  info = mconcat
    [ Options.progDesc "Lint code"
    ]

  parse = do
    -- TODO: Make `paths` and `stdin` mutually exclusive

    let parseHaskell = do
          paths <-
            many . Options.strArgument . mconcat $
              [ Options.metavar "PATHS"
              , Options.help "Only lint specific paths"
              ]
          stdin <-
            Options.switch . mconcat $
              [ Options.long "stdin"
              , Options.help "Lint code piped to `stdin`"
              ]
          pure $ Haskell HaskellOptions{ paths, stdin }

    command <-
      optional . Options.hsubparser . mconcat $
        [ Options.command "haskell" $ Options.info parseHaskell . mconcat $
            [ Options.progDesc "Lint Haskell code"
            ]
        ]

    pure Options{ command }

run :: Options -> IO ()
run _options = pure ()
