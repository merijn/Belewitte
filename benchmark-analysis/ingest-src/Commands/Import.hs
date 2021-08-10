{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Import (commands) where

import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as C
import Data.Proxy (Proxy(Proxy))

import Core
import Options
import Schema
import Sql (Filter, runRegion, runTransaction)
import Sql.Import

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
  { commandName = "import"
  , commandHeaderDesc = "list database entries"
  , commandDesc = ""
  } [ SingleCommand CommandInfo
        { commandName = "external-impls"
        , commandHeaderDesc = "Import external implementations"
        , commandDesc =
            "Import external implementations from an existing database."
        }
        $ importAll (Proxy :: Proxy ExternalImpl) <$> importDatabase
    , SingleCommand CommandInfo
        { commandName = "external-timers"
        , commandHeaderDesc = "Import external implementation timer data"
        , commandDesc =
            "Import timer data of external implementations from an existing \
            \database."
        }
        $ importAll (Proxy :: Proxy ExternalTimer) <$> importDatabase
    ]
  where
    importDatabase = strArgument $ mconcat
        [ metavar "DATABASE", help "Database to import from." ]

importAll :: forall proxy rec . Importable rec => proxy rec -> Text -> SqlM ()
importAll _ db = runImport db . runTransaction . runRegion . runConduit $ do
    selectSourceImport ([] :: [Filter rec]) [] .| C.mapM_ (lift . importEntity)
