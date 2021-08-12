{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Query (commands) where

import Control.Monad ((>=>))
import Data.Conduit ((.|), yield)
import qualified Data.Conduit.Combinators as C
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Core
import FormattedOutput
import Options
import Pretty.Fields (PrettyFields, prettyDouble_)
import Schema
import Sql
    (ColumnFilter(..), Max(..), MonadSql, SqlBackend, ToBackendKey, (==.))
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
        $ (>>= renderGraph) <$> entityParser
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
    , SingleCommand CommandInfo
        { commandName = "variant-properties"
        , commandHeaderDesc = "list graph properties for variant"
        , commandDesc = "Show information about a registered variant."
        }
        $ (>>= renderVariant) <$> entityParser
    ]
  where
    renderEntityParser
        :: forall proxy v
         . (PrettyFields (Entity v), ToBackendKey SqlBackend v)
        => proxy v -> Parser (SqlM ())
    renderEntityParser _ = (>>= printProxyEntity proxy) <$> entityParser
      where
        proxy :: Proxy (Entity v)
        proxy = Proxy

    renderVariant :: Entity Variant -> SqlM ()
    renderVariant = graphFromVariant >=> renderGraph

    graphFromVariant :: Entity Variant -> SqlM (Entity Graph)
    graphFromVariant = Sql.getJustEntity . variantGraphId . entityVal

renderGraph :: Entity Graph -> SqlM ()
renderGraph graph = renderRegionOutput $ do
    renderEntity graph >>= yield
    (_, Max maxLen) <- Sql.getFieldLengthWhere PropertyNameProperty
            [ Sql.ColumnEq PropertyNameIsStepProp False]

    Sql.selectSourceRegion [GraphPropValueGraphId ==. entityKey graph] []
        .| C.mapM (renderProperty maxLen)
  where
    renderProperty :: MonadSql m => Int -> Entity GraphPropValue -> m Text
    renderProperty maxLen (Entity _ GraphPropValue{..}) = do
        name <- propertyNameProperty <$> Sql.getJust graphPropValuePropId
        return $
            padText (name <> ":") <> prettyDouble_ graphPropValueValue <> "\n"
      where
        padText t = t <> T.replicate (maxLen + 2 - T.length t) " "

showRunCommand :: SqlM ()
showRunCommand = do
    result <- Sql.getGlobalVar RunCommand
    liftIO $ case result of
        Just cmd -> T.putStrLn $ "Run Command: " <> cmd
        Nothing -> putStrLn "Run command not set!"
