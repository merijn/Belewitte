module Commands.Set (commands) where

import qualified Control.Monad.Catch as Except
import System.Exit (exitFailure)

import Core
import OptionParsers
import Schema
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
    ]
  where
    runCmd :: Mod ArgumentFields a
    runCmd = metavar "COMMAND" <> help "Run command"

    forceOpt :: Parser Force
    forceOpt = flag NoForce Force $ mconcat
        [ long "force", help "Override pre-existing values when setting." ]


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
