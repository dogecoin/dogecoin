{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.TxSubmit.Tracing.ToObjectOrphans () where

import           Cardano.BM.Data.Severity (Severity (Debug, Error, Notice, Warning))
import           Cardano.BM.Data.Tracer (HasPrivacyAnnotation, HasSeverityAnnotation (..),
                   HasTextFormatter, ToObject (toObject), Transformable (..), mkObject,
                   trStructured)
import           Data.Aeson ((.=))
import           Data.String (String)
import           Data.Text (Text)
import           Ouroboros.Network.NodeToClient (ErrorPolicyTrace (..), WithAddr (..))
import           System.IO (IO)
import           Text.Show (Show (..))

import qualified Network.Socket as Socket

instance HasPrivacyAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace)
instance HasSeverityAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  getSeverityAnnotation (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error

instance HasTextFormatter (WithAddr Socket.SockAddr ErrorPolicyTrace) where

-- transform @ErrorPolicyTrace@
instance Transformable Text IO (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  trTransformer verb tr = trStructured verb tr

instance ToObject (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= ("ErrorPolicyTrace" :: String)
             , "address" .= show addr
             , "event" .= show ev ]
