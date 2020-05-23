{-# LANGUAGE FlexibleContexts #-}
module Commands.Reset (commands) where

import Core
import Options
import Schema
import Sql (MonadSql, Filter, (=.))
import qualified Sql
import qualified Sql.Transaction as SqlTrans

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
resetProperties = SqlTrans.runTransaction $ do
    SqlTrans.deleteWhere ([] :: [Filter GraphPropValue])
    SqlTrans.deleteWhere ([] :: [Filter StepPropValue])
    SqlTrans.updateWhere [] [VariantPropsStored =. False]

resetMeasurements :: MonadSql m => m ()
resetMeasurements = SqlTrans.runTransaction $ do
    SqlTrans.deleteWhere ([] :: [Filter StepTimer])
    SqlTrans.deleteWhere ([] :: [Filter TotalTimer])
    SqlTrans.deleteWhere ([] :: [Filter Run])

resetModels :: MonadSql m => m ()
resetModels = SqlTrans.runTransaction $ do
    SqlTrans.deleteWhere ([] :: [Filter PredictionModel])
    SqlTrans.deleteWhere ([] :: [Filter ModelProperty])
    SqlTrans.deleteWhere ([] :: [Filter UnknownPrediction])
    SqlTrans.deleteWhere ([] :: [Filter UnknownPredictionSet])
