{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Unset (commands) where

import qualified Data.Text as T
import System.Exit (exitFailure)

import Core
import OptionParsers
import Schema
import Sql (SqlBackend, SqlRecord, ToBackendKey, (=.))
import qualified Sql

data Force = NoForce | Force deriving (Show, Eq)

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
    { commandName = "unset"
    , commandHeaderDesc = "unset optional values"
    , commandDesc = "Unset or delete optional values and entries."
    }
    [ SingleCommand CommandInfo
        { commandName = "run-command"
        , commandHeaderDesc = "unset alternate job running command"
        , commandDesc = "Switch to using the default SLURM srun runner."
        }
        $ pure unsetRunCommand
    , CommandGroup CommandInfo
        { commandName = "default"
        , commandHeaderDesc = "change registered defaults"
        , commandDesc = "Change default entries for database tables."
        }
        [ SingleCommand CommandInfo
            { commandName = "platform"
            , commandHeaderDesc =
                "stop using platform as default non-timing runs"
            , commandDesc =
                "Stop using this platform as default for output validation and \
                \registering properties."
            }
            $ unsetDefault "platform" PlatformIsDefault <$> keyParser
        , SingleCommand CommandInfo
            { commandName = "variant-config"
            , commandHeaderDesc =
                "stop using this variant config as default for plots"
            , commandDesc =
                "Stop using this variant config as default for plots."
            }
            $ unsetDefault "variant config" VariantConfigIsDefault
                       <$> keyParser
        ]
    ]
  where
    keyParser :: ToBackendKey SqlBackend v => Parser (Key v)
    keyParser = argument (toSqlKey <$> auto) $ mconcat
        [ metavar "ID", help "Id to change." ]

unsetRunCommand :: SqlM ()
unsetRunCommand = Sql.unsetGlobalVar RunCommand

unsetDefault
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> EntityField r Bool -> Key r -> SqlM ()
unsetDefault name field key = do
    result <- Sql.getEntity key
    case result of
        Nothing -> logErrorN invalidKey >> liftIO exitFailure
        Just _ -> Sql.update key [field =. False]
  where
    invalidKey :: Text
    invalidKey = mconcat ["No ", T.pack name, " with id #", showSqlKey key]
