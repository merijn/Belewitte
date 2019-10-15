module Commands.Unset (commands) where

import Core
import OptionParsers
import Schema
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
    ]

unsetRunCommand :: SqlM ()
unsetRunCommand = Sql.unsetGlobalVar RunCommand
