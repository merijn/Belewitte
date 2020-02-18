{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Query (commands) where

import Data.Proxy (Proxy(..))
import qualified Data.Text.IO as T

import Core
import FormattedOutput
import Options
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
        $ renderEntityParser (Proxy :: Proxy Algorithm)
    , SingleCommand CommandInfo
        { commandName = "dataset"
        , commandHeaderDesc = "list dataset information"
        , commandDesc = "Show information about a registered dataset."
        }
        $ renderEntityParser (Proxy :: Proxy Dataset)
    , SingleCommand CommandInfo
        { commandName = "external-impl"
        , commandHeaderDesc = "list external implementation information"
        , commandDesc =
            "Show information about a registered external implementation."
        }
        $ renderEntityParser (Proxy :: Proxy ExternalImpl)
    , SingleCommand CommandInfo
        { commandName = "graph"
        , commandHeaderDesc = "list graph information"
        , commandDesc = "Show information about a registered graph."
        }
        $ renderEntityParser (Proxy :: Proxy Graph)
    , SingleCommand CommandInfo
        { commandName = "implementation"
        , commandHeaderDesc = "list implementation information"
        , commandDesc = "Show information about a registered implementation."
        }
        $ renderEntityParser (Proxy :: Proxy Implementation)
    , SingleCommand CommandInfo
        { commandName = "platform"
        , commandHeaderDesc = "list platform information"
        , commandDesc = "Show information about a registered platform."
        }
        $ renderEntityParser (Proxy :: Proxy Platform)
    , SingleCommand CommandInfo
        { commandName = "run-config"
        , commandHeaderDesc = "list run confg information"
        , commandDesc = "Show information about a registered run config."
        }
        $ renderEntityParser (Proxy :: Proxy RunConfig)
    , SingleCommand CommandInfo
        { commandName = "run"
        , commandHeaderDesc = "list run information"
        , commandDesc = "Show information about a registered run."
        }
        $ renderEntityParser (Proxy :: Proxy Run)
    , SingleCommand CommandInfo
        { commandName = "variant-config"
        , commandHeaderDesc = "list variant config information"
        , commandDesc = "Show information about a registered variant config."
        }
        $ renderEntityParser (Proxy :: Proxy VariantConfig)
    , SingleCommand CommandInfo
        { commandName = "variant"
        , commandHeaderDesc = "list variant information"
        , commandDesc = "Show information about a registered variant."
        }
        $ renderEntityParser (Proxy :: Proxy Variant)
    ]
  where
    renderEntityParser
        :: (PrettyFields v, ToBackendKey SqlBackend v)
        => proxy v -> Parser (SqlM ())
    renderEntityParser proxy = (>>= renderProxyEntity proxy) <$> entityParser

showRunCommand :: SqlM ()
showRunCommand = do
    result <- Sql.getGlobalVar RunCommand
    liftIO $ case result of
        Just cmd -> T.putStrLn $ "Run Command: " <> cmd
        Nothing -> putStrLn "Run command not set!"
