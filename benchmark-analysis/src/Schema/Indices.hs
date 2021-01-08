{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Schema.Indices (updateIndicesToVersion) where

import Data.Interval (Extended(Finite), Interval, (<=..<=))
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IM
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)

import Schema.Utils (Int64, MonadSql, Transaction)
import qualified Schema.Utils as Utils
import Schema.Version (schemaVersion)

(.=) :: MonadSql m
     => Interval Int64
     -> Text
     -> (Interval Int64, Transaction m ())
n .= act = (n, Utils.executeSql act)
infix 1 .=

currentVersion :: Extended Int64
currentVersion = Finite schemaVersion

updateIndicesToVersion :: MonadSql m => Int64 -> Transaction m ()
updateIndicesToVersion n = case IM.lookup n schemaIndices of
    Nothing -> return ()
    Just act -> act
  where
    schemaIndices :: MonadSql m => IntervalMap Int64 (Transaction m ())
    schemaIndices = IM.fromListWith (>>)
        [ 9 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueRunConfig"
ON "RunConfig"("id", "algorithmId")
|]
        , 9 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueVariantConfig"
ON "VariantConfig"("id", "algorithmId")
|]
        , 9 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueVariant"
ON "Variant"("id", "algorithmId")
|]
        , 9 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueImplementation"
ON "Implementation"("id", "algorithmId")
|]
        , 9 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueExternalImpl"
ON "ExternalImpl"("id", "algorithmId")
|]
        , 9 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniquePredictionModel"
ON "PredictionModel"("id", "algorithmId")
|]
        , 9 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueUnknownPrediction"
ON "UnknownPrediction"("id", "algorithmId")
|]
        , 19 <=..<= currentVersion .= [i|
CREATE TRIGGER IF NOT EXISTS "PredictionModelTrainingTypeInsert"
BEFORE INSERT ON "PredictionModel"
FOR EACH ROW
BEGIN
SELECT RAISE(ROLLBACK, "Models should be trained with only one training mode.")
WHERE ("NEW"."legacyTrainFraction" = 0) =
    ( "NEW"."trainGraphs" = 0
    AND "NEW"."trainVariants" = 0
    AND "NEW"."trainSteps" = 0
    );
END
|]
        , 21 <=..<= currentVersion .= [i|
CREATE UNIQUE INDEX IF NOT EXISTS "ForeignUniqueRun"
ON "Run"("id", "variantId")
|]

        , 22 <=..<= currentVersion .= [i|
CREATE INDEX IF NOT EXISTS "StepInfoQueryIndex"
ON "StepTimer"("variantId", "stepId")
|]
        , 29 <=..<= currentVersion .= [i|
CREATE TRIGGER IF NOT EXISTS "VariantConfigInsertUniqueness"
BEFORE INSERT ON "VariantConfig"
WHEN NEW."flags" IS NULL
BEGIN
SELECT RAISE(ROLLBACK, "There can be only one default variant config.")
FROM "VariantConfig"
WHERE "algorithmId" = NEW."algorithmId" AND "flags" IS NULL;
END
|]

        , 29 <=..<= currentVersion .= [i|
CREATE TRIGGER IF NOT EXISTS "VariantConfigUpdateUniqueness"
BEFORE UPDATE ON "VariantConfig"
WHEN NEW."flags" IS NULL
BEGIN
SELECT RAISE(ROLLBACK, "There can be only one default variant config.")
FROM "VariantConfig"
WHERE "algorithmId" = NEW."algorithmId" AND "flags" IS NULL;
END
|]
        ]
