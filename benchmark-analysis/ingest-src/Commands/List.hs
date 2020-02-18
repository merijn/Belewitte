{-# LANGUAGE MonadFailDesugaring #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands.List (commands) where

import Core
import Options
import Pretty.List
    ( AnyField(..)
    , Field(..)
    , FieldSpec(..)
    , (=.)
    , buildOptions
    , buildOptionsWithoutId
    )
import Schema

commands :: Command (SqlM ())
commands = CommandGroup CommandInfo
  { commandName = "list"
  , commandHeaderDesc = "list database entries"
  , commandDesc =
      "List database entries of various information stored in the database."
  } [ SingleCommand CommandInfo
        { commandName = "algorithm"
        , commandHeaderDesc = "list registered algorithms"
        , commandDesc = "List all registered algorithms."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple AlgorithmName
            , "pretty-name" =. StringField 'p' $ Optional AlgorithmPrettyName
            ]
    , SingleCommand CommandInfo
        { commandName = "dataset"
        , commandHeaderDesc = "list registered datasets"
        , commandDesc = "List all registered datasets."
        }
        $ buildOptions ([] :: [(String, FieldSpec Dataset)])
    , SingleCommand CommandInfo
        { commandName = "external-impl"
        , commandHeaderDesc = "list external implementations"
        , commandDesc = "List all external implementations."
        }
        $ buildOptions
            [ "algorith" =. IdField 'a' $ Simple ExternalImplAlgorithmId
            , "name" =. StringField 'n' $ Simple ExternalImplName
            , "pretty-name" =. StringField 'p' $ Optional ExternalImplPrettyName
            ]
    , SingleCommand CommandInfo
        { commandName = "external-timer"
        , commandHeaderDesc = "list timings of external implementations"
        , commandDesc = "List all timings of external implementations."
        }
        $ buildOptionsWithoutId
            [ AnyField ExternalTimerPlatformId
            , AnyField ExternalTimerVariantId
            , AnyField ExternalTimerImplId
            ]

            [ "platform" =. IdField 'p' $ Simple ExternalTimerPlatformId
            , "variant" =. IdField 'v' $ Simple ExternalTimerVariantId
            , "implementation" =. IdField 'i' $ Simple ExternalTimerImplId
            , "algorithm" =. IdField 'a' $ Simple ExternalTimerAlgorithmId
            , "name" =. StringField 'n' $ Simple ExternalTimerName
            , "min" =. SortOnlyField $ ExternalTimerMinTime
            , "avg" =. SortOnlyField $ ExternalTimerAvgTime
            , "max" =. SortOnlyField $ ExternalTimerMaxTime
            , "std-dev" =. SortOnlyField $ ExternalTimerStdDev
            ]
    , SingleCommand CommandInfo
        { commandName = "graph"
        , commandHeaderDesc = "list graphs"
        , commandDesc = "List all graphs."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple GraphName
            , "dataset" =. IdField 'd' $ Simple GraphDatasetId
            , "pretty-name" =. StringField 'r' $ Optional GraphPrettyName
            , "path" =. StringField 'p' $ Simple GraphPath
            ]
    , SingleCommand CommandInfo
        { commandName = "implementation"
        , commandHeaderDesc = "list implementations"
        , commandDesc = "List all implementations."
        }
        $ buildOptions
            [ "algorithm" =. IdField 'a' $ Simple ImplementationAlgorithmId
            , "name" =. StringField 'n' $ Simple ImplementationName
            , "type" =. EnumField 't' $ Simple ImplementationType
            , "pretty-name" =. StringField 'p' $ Optional ImplementationPrettyName
            ]
    , SingleCommand CommandInfo
        { commandName = "platform"
        , commandHeaderDesc = "list platforms"
        , commandDesc = "List all platforms."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple PlatformName
            , "pretty-name" =. StringField 'p' $ Optional PlatformPrettyName
            , "available" =. SortOnlyField $ PlatformAvailable
            , "default" =. SortOnlyField $ PlatformIsDefault
            ]
    , SingleCommand CommandInfo
        { commandName = "graph-properties"
        , commandHeaderDesc = "list graph properties"
        , commandDesc = "List all graph properties."
        }
        $ buildOptionsWithoutId
            [AnyField GraphPropGraphId, AnyField GraphPropProperty]

            [ "graph" =. IdField 'g' $ Simple GraphPropGraphId
            , "property" =. StringField 'p' $ Simple GraphPropProperty
            , "value" =. SortOnlyField $ GraphPropValue
            ]
    , SingleCommand CommandInfo
        { commandName = "step-properties"
        , commandHeaderDesc = "list step properties"
        , commandDesc = "List all step properties."
        }
        $ buildOptionsWithoutId
            [ AnyField StepPropValueAlgorithmId
            , AnyField StepPropValueVariantId
            , AnyField StepPropValueStepId
            , AnyField StepPropValueProperty
            ]

            [ "algorithm" =. IdField 'a' $ Simple StepPropValueAlgorithmId
            , "variant" =. IdField 'v' $ Simple StepPropValueVariantId
            , "step" =. IntField 's' $ Simple StepPropValueStepId
            , "property" =. StringField 'p' $ Simple StepPropValueProperty
            , "value" =. SortOnlyField $ StepPropValueValue
            ]
    , SingleCommand CommandInfo
        { commandName = "run-config"
        , commandHeaderDesc = "list run configs"
        , commandDesc = "List all run configs."
        }
        $ buildOptions
            [ "algorithm" =. IdField 'a' $ Simple RunConfigAlgorithmId
            , "platform" =. IdField 'p' $ Simple RunConfigPlatformId
            , "dataset" =. IdField 'd' $ Simple RunConfigDatasetId
            , "commit" =. StringField 'c' $
                    Converted RunConfigAlgorithmVersion CommitId
            , "repeats" =. SortOnlyField $ RunConfigRepeats
            ]
    , SingleCommand CommandInfo
        { commandName = "run"
        , commandHeaderDesc = "list runs"
        , commandDesc = "List all runs."
        }
        $ buildOptions
            [ "run-config" =. IdField 'r' $ Simple RunRunConfigId
            , "algorithm" =. IdField 'a' $ Simple RunAlgorithmId
            , "variant" =. IdField 'v' $ Simple RunVariantId
            , "implementation" =. IdField 'i' $ Simple RunImplId
            , "validated" =. EnumField 'l' $ Simple RunValidated
            , "time" =. SortOnlyField $ RunTimestamp
            ]
    , SingleCommand CommandInfo
        { commandName = "timer"
        , commandHeaderDesc = "list global timers"
        , commandDesc = "List all global timers."
        }
        $ buildOptionsWithoutId
            [AnyField TotalTimerRunId, AnyField TotalTimerName]

            [ "run" =. IdField 'r' $ Simple TotalTimerRunId
            , "name" =. StringField 'n' $ Simple TotalTimerName
            , "min" =. SortOnlyField $ TotalTimerMinTime
            , "avg" =. SortOnlyField $ TotalTimerAvgTime
            , "max" =. SortOnlyField $ TotalTimerMaxTime
            , "stddev" =. SortOnlyField $ TotalTimerStdDev
            ]
    , SingleCommand CommandInfo
        { commandName = "step-timer"
        , commandHeaderDesc = "list step timers"
        , commandDesc = "List all step timers."
        }
        $ buildOptionsWithoutId
            [ AnyField StepTimerRunId
            , AnyField StepTimerStepId
            , AnyField StepTimerName
            ]

            [ "run" =. IdField 'r' $ Simple StepTimerRunId
            , "step" =. IntField 's' $ Simple StepTimerStepId
            , "name" =. StringField 'n' $ Simple StepTimerName
            , "min" =. SortOnlyField $ StepTimerMinTime
            , "avg" =. SortOnlyField $ StepTimerAvgTime
            , "max" =. SortOnlyField $ StepTimerMaxTime
            , "stddev" =. SortOnlyField $ StepTimerStdDev
            ]
    , SingleCommand CommandInfo
        { commandName = "variant-config"
        , commandHeaderDesc = "list variant configs"
        , commandDesc = "List all variant configs."
        }
        $ buildOptions
            [ "name" =. StringField 'n' $ Simple VariantConfigName
            , "algorithm" =. IdField 'a' $ Simple VariantConfigAlgorithmId
            , "default" =. SortOnlyField $ VariantConfigIsDefault
            ]
    , SingleCommand CommandInfo
        { commandName = "variant"
        , commandHeaderDesc = "list registered variant"
        , commandDesc = "List all registered variants."
        }
        $ buildOptions
            [ "variantconfig" =. IdField 'v' $ Simple VariantVariantConfigId
            , "algorithm" =. IdField 'a' $ Simple VariantAlgorithmId
            , "graph" =. IdField 'g' $ Simple VariantGraphId
            , "props-stored" =. EnumField 'p' $ Simple VariantPropsStored
            , "retries" =. SortOnlyField $ VariantRetryCount
            ]
    ]
