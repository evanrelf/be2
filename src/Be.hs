{-# LANGUAGE ApplicativeDo #-}

module Be (main) where

import Be.Format qualified as Format
import Be.Lint qualified as Lint
import Options.Applicative qualified as Options

data Options = Options
  { command :: Command
  }

data Command
  = Format Format.Options
  | Lint Lint.Options
  | Clean

parseOptions :: Options.Parser Options
parseOptions = do
  command <-
    Options.hsubparser . mconcat $
      [ Options.command "format" $ fmap Format Format.parserInfo
      , Options.command "lint" $ fmap Lint Lint.parserInfo
      , Options.command "clean" $
          Options.info (pure Clean) (Options.progDesc "Delete cache")
      ]

  pure Options{ command }

getOptions :: IO Options
getOptions = do
  let parserPrefs =
        Options.prefs $ mconcat
          [ Options.showHelpOnError
          ]
  let parserInfo =
        Options.info (Options.helper <*> parseOptions) . mconcat $
          [ Options.header "Evan's bespoke backend tooling"
          ]
  Options.customExecParser parserPrefs parserInfo

main :: IO ()
main = do
  options <- getOptions

  case options.command of
    Format formatOptions -> Format.run formatOptions
    Lint lintOptions -> Lint.run lintOptions
    Clean -> pure ()
