{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Query (commands) where

import Data.Proxy (Proxy(..))
import qualified Data.Text.IO as T
import System.Exit (exitFailure)

import Core
import FormattedOutput
import OptionParsers
import Pretty.Fields (PrettyFields)
import Schema
import Sql (SqlBackend, ToBackendKey)
import qualified Sql

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
    { commandName = "query"
    , commandHeaderDesc = "list information of registered entries"
    , commandDesc = "Show information about a registered entry."
    }
    [ SingleCommand CommandInfo
        { commandName = "run-command"
        , commandHeaderDesc = "show alternate job running command"
        , commandDesc =
            "Show replacement of SLURM's srun as job runner command."
        }
        $ pure showRunCommand
    , SingleCommand CommandInfo
        { commandName = "algorithm"
        , commandHeaderDesc = "list algorithm information"
        , commandDesc = "Show information about a registered algorithm."
        }
        $ queryDatabase (Proxy :: Proxy Algorithm)
    , SingleCommand CommandInfo
        { commandName = "dataset"
        , commandHeaderDesc = "list dataset information"
        , commandDesc = "Show information about a registered dataset."
        }
        $ queryDatabase (Proxy :: Proxy Dataset)
    , SingleCommand CommandInfo
        { commandName = "external-impl"
        , commandHeaderDesc = "list external implementation information"
        , commandDesc =
            "Show information about a registered external implementation."
        }
        $ queryDatabase (Proxy :: Proxy ExternalImpl)
    , SingleCommand CommandInfo
        { commandName = "graph"
        , commandHeaderDesc = "list graph information"
        , commandDesc = "Show information about a registered graph."
        }
        $ queryDatabase (Proxy :: Proxy Graph)
    , SingleCommand CommandInfo
        { commandName = "implementation"
        , commandHeaderDesc = "list implementation information"
        , commandDesc = "Show information about a registered implementation."
        }
        $ queryDatabase (Proxy :: Proxy Implementation)
    , SingleCommand CommandInfo
        { commandName = "platform"
        , commandHeaderDesc = "list platform information"
        , commandDesc = "Show information about a registered platform."
        }
        $ queryDatabase (Proxy :: Proxy Platform)
    , SingleCommand CommandInfo
        { commandName = "run-config"
        , commandHeaderDesc = "list run confg information"
        , commandDesc = "Show information about a registered run config."
        }
        $ queryDatabase (Proxy :: Proxy RunConfig)
    , SingleCommand CommandInfo
        { commandName = "run"
        , commandHeaderDesc = "list run information"
        , commandDesc = "Show information about a registered run."
        }
        $ queryDatabase (Proxy :: Proxy Run)
    , SingleCommand CommandInfo
        { commandName = "variant-config"
        , commandHeaderDesc = "list variant config information"
        , commandDesc = "Show information about a registered variant config."
        }
        $ queryDatabase (Proxy :: Proxy VariantConfig)
    , SingleCommand CommandInfo
        { commandName = "variant"
        , commandHeaderDesc = "list variant information"
        , commandDesc = "Show information about a registered variant."
        }
        $ queryDatabase (Proxy :: Proxy Variant)
    ]

showRunCommand :: SqlM ()
showRunCommand = do
    result <- Sql.getGlobalVar RunCommand
    liftIO $ case result of
        Just cmd -> T.putStrLn $ "Run Command: " <> cmd
        Nothing -> putStrLn "Run command not set!"

queryDatabase
    :: forall a proxy
     . (PrettyFields a, ToBackendKey SqlBackend a)
    => proxy a -> Parser (SqlM ())
queryDatabase _ = outputEntity <$> (keyParser :: Parser (Key a))

keyParser :: ToBackendKey SqlBackend v => Parser (Key v)
keyParser = argument (toSqlKey <$> auto) $ mconcat
    [ metavar "ID", help "Id to display." ]

outputEntity
    :: (PrettyFields a, ToBackendKey SqlBackend a) => Key a -> SqlM ()
outputEntity key = do
    result <- Sql.getEntity key
    case result of
        Nothing -> exitWithError
        Just v -> renderEntity v
  where
    exitWithError :: SqlM a
    exitWithError = logErrorN msg >> liftIO exitFailure

    msg :: Text
    msg = "No entity with id #" <> showSqlKey key
