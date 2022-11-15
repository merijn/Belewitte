{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Timers.V2 where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)

import qualified Schema.Utils as Utils

import Schema.Run (RunId)

Utils.mkEntities "schema" [persistUpperCase|
TotalTimer
    runId RunId
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    Primary runId name
    deriving Eq Show

StepTimer
    runId RunId
    stepId Int
    name Text
    minTime Double
    avgTime Double
    maxTime Double
    stdDev Double
    Primary runId stepId name
    deriving Eq Show
|]
