{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.Set (commands) where

import qualified Control.Monad.Catch as Except
import qualified Data.Text as T
import System.Exit (exitFailure)

import Core
import OptionParsers
import Schema
import Sql (SqlBackend, SqlRecord, ToBackendKey, (==.), (=.))
import qualified Sql

data Force = NoForce | Force deriving (Show, Eq)

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
    { commandName = "set"
    , commandHeaderDesc = "set values in the database"
    , commandDesc = "Set or override values in the database"
    }
    [ SingleCommand CommandInfo
        { commandName = "run-command"
        , commandHeaderDesc = "set alternate job running command"
        , commandDesc =
            "Replace the use of SLURM's srun command with an alternate job \
            \runner."
        }
        $ setRunCommand <$> forceOpt <*> strArgument runCmd
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
    ]
  where
    runCmd :: Mod ArgumentFields a
    runCmd = metavar "COMMAND" <> help "Run command"

    forceOpt :: Parser Force
    forceOpt = flag NoForce Force $ mconcat
        [ long "force", help "Override pre-existing values when setting." ]

    keyParser :: ToBackendKey SqlBackend v => Parser (Key v)
    keyParser = argument (toSqlKey <$> auto) $ mconcat
        [ metavar "ID", help "Id to change." ]

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

setDefault
    :: (SqlRecord r, ToBackendKey SqlBackend r)
    => String -> EntityField r Bool -> Force -> Key r -> SqlM ()
setDefault name field force key = do
    Sql.get key >>= checkExists

    numDefault <- Sql.count [field ==. True]
    case (numDefault, force) of
        (0, _) -> Sql.update key [field =. True]
        (_, Force) -> do
            Sql.updateWhere [] [field =. False]
            Sql.update key [field =. True]
        _ -> liftIO $ do
            putStrLn $ "Default " <> name <> " is already set!"
            putStrLn "Use --force to overwrite."
            exitFailure
  where
    checkExists :: Maybe a -> SqlM ()
    checkExists (Just _) = return ()
    checkExists Nothing = logErrorN msg >> liftIO exitFailure

    msg :: Text
    msg = mconcat ["No ", T.pack name, " with id #", showSqlKey key]
