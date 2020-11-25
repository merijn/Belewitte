{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Commands.Import (commands) where

import Control.Monad (void)
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as C

import Core
import Options
import Schema
import Sql (Filter, Region, (==.))
import qualified Sql
import Sql.Import

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
  { commandName = "import"
  , commandHeaderDesc = "list database entries"
  , commandDesc = ""
  } [ SingleCommand CommandInfo
        { commandName = "algorithms"
        , commandHeaderDesc = ""
        , commandDesc = ""
        }
        $ importAlgorithms <$> importDatabase
    , SingleCommand CommandInfo
        { commandName = "external-impls"
        , commandHeaderDesc = ""
        , commandDesc = ""
        }
        $ importExternalImpl <$> importDatabase
    , SingleCommand CommandInfo
        { commandName = "external-timers"
        , commandHeaderDesc = ""
        , commandDesc = ""
        }
        $ importExternalTimers <$> importDatabase
    ]
  where
    importDatabase = strArgument $ mconcat
        [ metavar "DATABASE", help "Database to import from." ]

importAlgorithms :: Text -> SqlM ()
importAlgorithms db = runImport db . runRegionConduit $ do
    selectSourceImport ([] :: [Filter Algorithm]) []
        .| C.mapM_ (void . Sql.insertUniq . entityVal)

importExternalImpl :: Text -> SqlM ()
importExternalImpl db = runImport db . runRegionConduit $ do
    selectSourceImport ([] :: [Filter ExternalImpl]) []
        .| C.mapM_ (void . Sql.insertUniq . entityVal)

importExternalTimers :: Text -> SqlM ()
importExternalTimers db = runImport db . runRegionConduit $ do
    selectSourceImport ([] :: [Filter ExternalImpl]) []
        .> streamExternalImpl
        .| C.mapM_ Sql.insert_

streamExternalImpl
    :: Entity ExternalImpl
    -> ConduitT (Entity ExternalImpl) ExternalTimer (Region (Import SqlM)) ()
streamExternalImpl Entity{..} = do
    newKey <- Sql.onlyUnique entityVal >>= Sql.getJustKeyBy
    selectSourceImport [ExternalTimerImplId ==. entityKey] []
        .| C.map (\(Entity _ timing) -> timing{externalTimerImplId = newKey })
