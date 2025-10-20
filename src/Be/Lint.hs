{-# LANGUAGE ApplicativeDo #-}

module Be.Lint
  ( Options (..)
  , parser
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
  deriving stock (Show)

data HaskellOptions = HaskellOptions
  { input :: Maybe Input
  }
  deriving stock (Show)

data Input
  = Paths (NonEmpty FilePath)
  | Stdin
  deriving stock (Show)

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

run :: Options -> IO ()
run options = do
  case options.command of
    Just (Haskell haskellOptions) -> runHaskell haskellOptions
    Nothing -> runHaskell HaskellOptions{ input = Nothing }

runHaskell :: HaskellOptions -> IO ()
runHaskell _options = do
  pure ()
