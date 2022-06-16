{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Check
  ( checkCommonErrors
  ) where

--import           Data.Text (Text)
--import qualified Data.Text as T

import           Cardano.Logging (SeverityS (..))

import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Notifications.Utils
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Types

checkCommonErrors
  :: NodeId
  -> TraceObjectInfo
  -> EventsQueues
  -> IO ()
checkCommonErrors nodeId (msg, severity, ts) eventsQueues =
  case severity of
    Error     -> addNewEvent eventsQueues EventErrors      $ Event nodeId ts msg
    Critical  -> addNewEvent eventsQueues EventCriticals   $ Event nodeId ts msg
    Alert     -> addNewEvent eventsQueues EventAlerts      $ Event nodeId ts msg
    Emergency -> addNewEvent eventsQueues EventEmergencies $ Event nodeId ts msg
    _         -> return ()


