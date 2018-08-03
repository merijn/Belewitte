{-# LANGUAGE OverloadedStrings #-}
module OptionParsers where

import Control.Monad.Logger (LogLevel(..), LogSource)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

databaseOption :: Parser Text
databaseOption = strOption . mconcat $
    [ metavar "DATABASE", short 'd', long "database"
    , value "benchmarks.db", help "Path of SQLite database to use."
    , showDefaultWith T.unpack
    ]

verbosityOption :: Parser (LogSource -> LogLevel -> Bool)
verbosityOption = logVerbosity . (levels !!) <$> verb
  where
    logVerbosity verbosity = \_ lvl -> lvl >= verbosity

    levels = LevelError : LevelWarn : LevelInfo : repeat LevelDebug

    verb = option auto . mconcat $
        [ short 'v', long "verbose", help "Enable more verbose logging."
        , value 0, metavar "N", showDefault ]
