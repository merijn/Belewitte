{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Schema.Indices (updateIndicesToVersion) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Interpolate.IsString (i)
import Schema.Utils (Int64, MonadSql, Transaction)
import qualified Schema.Utils as Utils

(.=) :: MonadSql m => Int64 -> Transaction m () -> (Int64, Transaction m ())
n .= act = (n, act)

updateIndicesToVersion :: MonadSql m => Int64 -> Transaction m ()
updateIndicesToVersion n = case M.lookupLE n schemaIndices of
    Nothing -> return ()
    Just (_, act) -> act
  where
    schemaIndices :: MonadSql m => Map Int64 (Transaction m ())
    schemaIndices = M.fromList
        [ 9 .= do
            Utils.executeSql [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueRunConfig"
ON "RunConfig"("id", "algorithmId")
|]

            Utils.executeSql [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueVariantConfig"
ON "VariantConfig"("id", "algorithmId")
|]

            Utils.executeSql [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueVariant"
ON "Variant"("id", "algorithmId")
|]

            Utils.executeSql [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueImplementation"
ON "Implementation"("id", "algorithmId")
|]

            Utils.executeSql [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueExternalImpl"
ON "ExternalImpl"("id", "algorithmId")
|]

            Utils.executeSql [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniquePredictionModel"
ON "PredictionModel"("id", "algorithmId")
|]

            Utils.executeSql [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueUnknownPrediction"
ON "UnknownPrediction"("id", "algorithmId")
|]
        ]
