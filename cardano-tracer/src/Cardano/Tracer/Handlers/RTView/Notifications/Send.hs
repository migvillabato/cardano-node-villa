{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Send
  ( makeAndSendNotification
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (flushTBQueue)
import           Control.Monad (void)
import qualified Data.Text as T

import           Cardano.Tracer.Handlers.RTView.Notifications.Email
import           Cardano.Tracer.Handlers.RTView.Notifications.Settings
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Utils

makeAndSendNotification :: EventsQueue -> IO ()
makeAndSendNotification eventsQueue = lift2M
  sendNotification
    readSavedEmailSettings
    $ atomically (flushTBQueue eventsQueue)

sendNotification
  :: EmailSettings
  -> [Event]
  -> IO ()
sendNotification _ [] = return ()
sendNotification emailSettings newEvents =
  void $ createAndSendEmail emailSettings body
 where
  body = preface <> events

  preface = T.intercalate nl
    [ "This is a notification from Cardano RTView service."
    , ""
    , "The following " <> (if onlyOne then "event" else "events") <> " occurred:"
    , ""
    ]

  events = T.intercalate nl
    ["[" <> showT ts <> "] [" <> showT nodeId <> "] [" <> showT msg <> "]" | Event nodeId ts msg <- newEvents]

  onlyOne = length newEvents == 1
