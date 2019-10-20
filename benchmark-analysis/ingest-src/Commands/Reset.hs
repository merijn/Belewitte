{-# LANGUAGE FlexibleContexts #-}
module Commands.Reset (commands) where

import Core
import OptionParsers
import Schema
import Sql (MonadSql, Filter, (=.))
import qualified Sql

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
    { commandName = "reset"
    , commandHeaderDesc = "commands for resetting database"
    , commandDesc = "Reset or drop database entries"
    }
    [ SingleCommand CommandInfo
        { commandName = "retries"
        , commandHeaderDesc = "resets the retry count"
        , commandDesc = "Reset the retry count for failed experiments"
        }
        $ pure resetRetries
    , SingleCommand CommandInfo
        { commandName = "results"
        , commandHeaderDesc = "resets the stored result hash"
        , commandDesc = "Reset the result hash for every variant"
        }
        $ pure resetResults
    , SingleCommand CommandInfo
        { commandName = "properties"
        , commandHeaderDesc = "deletes logged properties"
        , commandDesc = "Deletes the properties stored for each variant"
        }
        $ pure resetProperties
    , SingleCommand CommandInfo
        { commandName = "measurements"
        , commandHeaderDesc = "deletes timing measurements"
        , commandDesc = "Delete all timing measurements"
        }
        $ pure resetMeasurements
    , SingleCommand CommandInfo
        { commandName = "models"
        , commandHeaderDesc = "deletes models"
        , commandDesc = "Delete all stored models"
        }
        $ pure resetModels
    ]

resetRetries :: MonadSql m => m ()
resetRetries = Sql.updateWhere [] [VariantRetryCount =. 0]

resetResults :: MonadSql m => m ()
resetResults = Sql.updateWhere [] [VariantResult =. Nothing]

resetProperties :: MonadSql m => m ()
resetProperties = do
    Sql.deleteWhere ([] :: [Filter GraphProp])
    Sql.deleteWhere ([] :: [Filter StepProp])
    Sql.updateWhere [] [VariantPropsStored =. False]

resetMeasurements :: MonadSql m => m ()
resetMeasurements = do
    Sql.deleteWhere ([] :: [Filter StepTimer])
    Sql.deleteWhere ([] :: [Filter TotalTimer])
    Sql.deleteWhere ([] :: [Filter Run])

resetModels :: MonadSql m => m ()
resetModels = do
    Sql.deleteWhere ([] :: [Filter PredictionModel])
    Sql.deleteWhere ([] :: [Filter ModelGraphProperty])
    Sql.deleteWhere ([] :: [Filter ModelStepProperty])
    Sql.deleteWhere ([] :: [Filter UnknownPrediction])
    Sql.deleteWhere ([] :: [Filter UnknownSet])
