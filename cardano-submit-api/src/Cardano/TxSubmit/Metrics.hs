{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Metrics
  ( TxSubmitMetrics(..)
  , makeMetrics
  , registerMetricsServer
  ) where

import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Concurrent.Async (Async, async)
import           Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import           Data.Function (($), (.))
import           Data.Monoid (Monoid (mempty))
import           System.IO (IO)
import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (..), registerGauge,
                   runRegistryT, unRegistryT)
import           System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import           System.Metrics.Prometheus.Metric.Gauge (Gauge)

newtype TxSubmitMetrics = TxSubmitMetrics
  { tsmCount :: Gauge
  }

registerMetricsServer :: IO (TxSubmitMetrics, Async ())
registerMetricsServer =
  runRegistryT $ do
    metrics <- makeMetrics
    registry <- RegistryT ask
    server <- liftIO . async $ runReaderT (unRegistryT $ serveMetricsT 8081 []) registry
    pure (metrics, server)

makeMetrics :: RegistryT IO TxSubmitMetrics
makeMetrics = TxSubmitMetrics <$> registerGauge "tx_submit_count" mempty
