{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Set (commands) where

import Control.Monad (guard)
import qualified Control.Monad.Catch as Except
import qualified Data.Text as T
import qualified Options.Applicative.Help.Pretty as Pretty
import System.Exit (exitFailure)

import Core
import Options
import Schema
import Sql (SqlBackend, SqlRecord, ToBackendKey, Transaction, (==.), (=.))
import qualified Sql
import qualified Sql.Transaction as SqlTrans

data Force = NoForce | Force deriving (Show, Eq)

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
    { commandName = "set"
    , commandHeaderDesc = "set values in the database"
    , commandDesc = "Set or override values in the database"
    }
    [ SingleCommand CommandInfoDoc
        { commandName = "run-command"
        , commandHeaderDesc = "set alternate job running command"
        , commandDocDesc = mconcat
            [ Pretty.text
                "Replace the use of SLURM's srun command with an alternate \
                \job runner. Runs are invoked as follows:"
            , Pretty.hardline, Pretty.hardline
            , Pretty.indent 4 $ Pretty.text
                "<run-command> <platform> -- kernel-runner <runner args...>"
            , Pretty.hardline, Pretty.hardline
            , Pretty.text
                "Here, the run-command is either a built-in srun invocation, \
                \or the command configured by the user using Ingest. The \
                \platform is either the flags specified for the platform (if \
                \any) and otherwise the name specified during the \
                \registration with Ingest."
            ]
        }
        $ setRunCommand <$> forceOpt <*> strArgument runCmd
    , SingleCommand CommandInfo
        { commandName = "available"
        , commandHeaderDesc = "set number of available machines for platform"
        , commandDesc =
            "Set the number of available machines (and thus maximum parallel \
            \jobs) for a platform."
        }
        $ setAvailable <$> keyParser <*> numNodes
    , CommandGroup CommandInfo
        { commandName = "default"
        , commandHeaderDesc = "change registered defaults"
        , commandDesc = "Change default entries for database tables."
        }
        [ SingleCommand CommandInfo
            { commandName = "platform"
            , commandHeaderDesc =
                "change which platform is used for non-timing runs"
            , commandDesc =
                "Changes which platform is used for output validation and \
                \registering properties."
            }
            $ setDefault "platform" PlatformIsDefault
                     <$> forceOpt <*> keyParser
        , SingleCommand CommandInfo
            { commandName = "variant-config"
            , commandHeaderDesc = "change which config is plotted by default"
            , commandDesc =
                "Changes which variant is shown by default in plots."
            }
            $ setDefault "variant config" VariantConfigIsDefault
                     <$> forceOpt <*> keyParser
        ]
    , CommandGroup CommandInfo
        { commandName = "flags"
        , commandHeaderDesc = "change registered flags"
        , commandDesc =
            "Change the registered flags for database entries."
        }
        [ SingleCommand CommandInfo
            { commandName = "platform"
            , commandHeaderDesc = "change platform flags"
            , commandDesc =
              "Changes the flags used to run on the given platform"
            }
            $ setFlags "platform" PlatformFlags
                <$> forceOpt <*> keyParser <*> flags
        ]
    , CommandGroup CommandInfo
        { commandName = "pretty-name"
        , commandHeaderDesc = "change registered pretty name"
        , commandDesc =
            "Change the registered pretty name for database entries."
        }
        [ SingleCommand CommandInfo
            { commandName = "algorithm"
            , commandHeaderDesc = "change algorithm pretty name"
            , commandDesc = "Changes the pretty name for an algorithm."
            }
            $ setPrettyName "algorithm" AlgorithmPrettyName
                <$> forceOpt <*> keyParser <*> prettyName
        , SingleCommand CommandInfo
            { commandName = "implementation"
            , commandHeaderDesc = "change implementation pretty name"
            , commandDesc = "Changes the pretty name for an implementation."
            }
            $ setPrettyName "implementation" ImplementationPrettyName
                <$> forceOpt <*> keyParser <*> prettyName
        , SingleCommand CommandInfo
            { commandName = "external-impl"
            , commandHeaderDesc = "change external implementation pretty name"
            , commandDesc =
                "Changes the pretty name for an external implementation."
            }
            $ setPrettyName "external implementation" ExternalImplPrettyName
                <$> forceOpt <*> keyParser <*> prettyName
        , SingleCommand CommandInfo
            { commandName = "graph"
            , commandHeaderDesc = "change graph pretty name"
            , commandDesc = "Changes the pretty name for a graph."
            }
            $ setPrettyName "graph" GraphPrettyName
                <$> forceOpt <*> keyParser <*> prettyName
        , SingleCommand CommandInfo
            { commandName = "model"
            , commandHeaderDesc = "change model pretty name"
            , commandDesc = "Changes the pretty name for a model."
            }
            $ setPrettyName "model" PredictionModelPrettyName
                <$> forceOpt <*> keyParser <*> prettyName
        , SingleCommand CommandInfo
            { commandName = "platform"
            , commandHeaderDesc = "change platform pretty name"
            , commandDesc = "Changes the pretty name for a platform."
            }
            $ setPrettyName "platform" PlatformPrettyName
                <$> forceOpt <*> keyParser <*> prettyName
        ]
    ]
  where
    runCmd :: Mod ArgumentFields a
    runCmd = metavar "COMMAND" <> help "Run command"

    numNodes :: Parser Int
    numNodes = argument (auto >>= checkPositive) $ mconcat
        [ metavar "N", help "Available machine count." ]
      where
        -- FIXME report error
        checkPositive n = n <$ guard (n > 0)

    forceOpt :: Parser Force
    forceOpt = flag NoForce Force $ mconcat
        [ long "force", help "Override pre-existing values when setting." ]

    keyParser :: ToBackendKey SqlBackend v => Parser (Key v)
    keyParser = argument (toSqlKey <$> auto) $ mconcat
        [ metavar "ID", help "Id to change." ]

    flags :: Parser Text
    flags = strArgument $ mconcat
        [ metavar "FLAGS", help "Flags value to set." ]

    prettyName :: Parser Text
    prettyName = strArgument $ mconcat
        [ metavar "NAME", help "Pretty name to set." ]

setRunCommand :: Force -> Text -> SqlM ()
setRunCommand force = setCommand
  where
    setCommand :: Text -> SqlM ()
    setCommand = case force of
        NoForce -> Except.handle sqliteException
                    . Sql.initialiseGlobalVar RunCommand

        Force -> Sql.setGlobalVar RunCommand

    sqliteException :: (MonadIO m, MonadThrow m) => SqliteException -> m ()
    sqliteException SqliteException{seError = ErrorConstraint} = liftIO $ do
        putStrLn "Run command is already set!\nUse --force to overwrite."
        exitFailure

    sqliteException e = Except.throwM e

setAvailable :: Key Platform -> Int -> SqlM ()
setAvailable key n = withCheckedKey "platform" key $ \_ -> do
    SqlTrans.update key [PlatformAvailable =. n]

withCheckedKey
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> Key r -> (Entity r -> Transaction SqlM ()) -> SqlM ()
withCheckedKey name key act = SqlTrans.runTransaction $ do
    ent <- SqlTrans.getEntity key >>= checkExists
    act ent
  where
    checkExists :: Maybe (Entity a) -> Transaction SqlM (Entity a)
    checkExists = maybe (logErrorN msg >> liftIO exitFailure) return

    msg :: Text
    msg = mconcat ["No ", T.pack name, " with id #", showSqlKey key]

setPrettyName
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String
    -> EntityField r (Maybe Text)
    -> Force
    -> Key r
    -> Text
    -> SqlM ()
setPrettyName name field force key val = withCheckedKey name key $ \ent -> do
    case (SqlTrans.fieldFromEntity field ent, force) of
        (Nothing, _) -> SqlTrans.update (entityKey ent) [field =. Just val]
        (_, Force) -> SqlTrans.update (entityKey ent) [field =. Just val]
        _ -> liftIO $ do
            putStrLn $ "Pretty name for " <> name <> " is already set!"
            putStrLn "Use --force to overwrite!"
            exitFailure

setFlags
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String
    -> EntityField r (Maybe Text)
    -> Force
    -> Key r
    -> Text
    -> SqlM ()
setFlags name field force key val = withCheckedKey name key $ \ent -> do
    case (SqlTrans.fieldFromEntity field ent, force) of
        (Nothing, _) -> SqlTrans.update (entityKey ent) [field =. Just val]
        (_, Force) -> SqlTrans.update (entityKey ent) [field =. Just val]
        _ -> liftIO $ do
            putStrLn $ "Flags for " <> name <> " are already set!"
            putStrLn "Use --force to overwrite!"
            exitFailure

setDefault
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> EntityField r Checkmark -> Force -> Key r -> SqlM ()
setDefault name field force key = withCheckedKey name key $ \ent -> do
    numDefault <- SqlTrans.count [field ==. Active]
    case (numDefault, force) of
        (0, _) -> SqlTrans.update (entityKey ent) [field =. Active]
        (_, Force) -> do
            SqlTrans.updateWhere [] [field =. Inactive]
            SqlTrans.update (entityKey ent) [field =. Active]
        _ -> liftIO $ do
            putStrLn $ "Default " <> name <> " is already set!"
            putStrLn "Use --force to overwrite."
            exitFailure
