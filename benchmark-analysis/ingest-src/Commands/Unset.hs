{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Unset (commands) where

import qualified Data.Text as T
import System.Exit (exitFailure)

import Core
import Options
import Schema
import Sql (SqlBackend, SqlRecord, ToBackendKey, Transaction, (=.))
import qualified Sql
import qualified Sql.Transaction as SqlTrans

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
    , CommandGroup CommandInfo
        { commandName = "flags"
        , commandHeaderDesc = "unset registered flags"
        , commandDesc = "Unset the registered flags for database entries."
        }
        [ SingleCommand CommandInfo
            { commandName = "platform"
            , commandHeaderDesc = "unset platform flags"
            , commandDesc = "Unset the flags for a platform."
            }
            $ unsetFlags "platform" PlatformFlags <$> keyParser
        ]
    , CommandGroup CommandInfo
        { commandName = "pretty-name"
        , commandHeaderDesc = "unset registered pretty name"
        , commandDesc = "Unset the registered pretty name for database entries."
        }
        [ SingleCommand CommandInfo
            { commandName = "algorithm"
            , commandHeaderDesc = "unset algorithm pretty name"
            , commandDesc = "Unset the pretty name for an algorithm."
            }
            $ unsetPrettyName "algorithm" AlgorithmPrettyName <$> keyParser
        , SingleCommand CommandInfo
            { commandName = "implementation"
            , commandHeaderDesc = "unset implementation pretty name"
            , commandDesc = "Unset the pretty name for an implementation."
            }
            $ unsetPrettyName "implementation" ImplementationPrettyName
                <$> keyParser
        , SingleCommand CommandInfo
            { commandName = "external-impl"
            , commandHeaderDesc = "unset external implementation pretty name"
            , commandDesc =
                "Unset the pretty name for an external implementation."
            }
            $ unsetPrettyName "external implementation" ExternalImplPrettyName
                <$> keyParser
        , SingleCommand CommandInfo
            { commandName = "graph"
            , commandHeaderDesc = "unset graph pretty name"
            , commandDesc = "Unset the pretty name for a graph."
            }
            $ unsetPrettyName "graph" GraphPrettyName <$> keyParser
        , SingleCommand CommandInfo
            { commandName = "platform"
            , commandHeaderDesc = "unset platform pretty name"
            , commandDesc = "Unset the pretty name for a platform."
            }
            $ unsetPrettyName "platform" PlatformPrettyName <$> keyParser
        ]
    ]
  where
    keyParser :: ToBackendKey SqlBackend v => Parser (Key v)
    keyParser = argument (toSqlKey <$> auto) $ mconcat
        [ metavar "ID", help "Id to change." ]

unsetRunCommand :: SqlM ()
unsetRunCommand = Sql.unsetGlobalVar RunCommand

unsetChecked
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> (Key r -> Transaction SqlM ()) -> Key r -> SqlM ()
unsetChecked name f key = SqlTrans.runTransaction $ do
    result <- SqlTrans.getEntity key
    case result of
        Nothing -> logErrorN invalidKey >> liftIO exitFailure
        Just _ -> f key
  where
    invalidKey :: Text
    invalidKey = mconcat ["No ", T.pack name, " with id #", showSqlKey key]

unsetDefault
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> EntityField r Checkmark -> Key r -> SqlM ()
unsetDefault name field = unsetChecked name $ \key ->
    SqlTrans.update key [field =. Inactive]

unsetFlags
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> EntityField r (Maybe Text) -> Key r -> SqlM ()
unsetFlags name field = unsetChecked name $ \key ->
    SqlTrans.update key [field =. Nothing]

unsetPrettyName
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> EntityField r (Maybe Text) -> Key r -> SqlM ()
unsetPrettyName name field = unsetChecked name $ \key ->
    SqlTrans.update key [field =. Nothing]
