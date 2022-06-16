{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Notifications
  ( getCurrentEmailSettings
  , restoreEmailSettings
  , restoreEventsSettings
  , saveEmailSettings
  , saveEventsSettings
  , setStatusTestEmailButton
  ) where

import           Control.Monad (unless)
import           Data.Maybe (fromMaybe)
import           Data.Text (pack, unpack)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.Notifications.Settings
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.UI.JS.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Utils

restoreEmailSettings :: Window -> UI ()
restoreEmailSettings window = do
  eSettings <- liftIO readSavedEmailSettings
  setEmailSettings eSettings
  setStatusTestEmailButton window
 where
  setEmailSettings settings = do
    setValue (unpack $ esSMTPHost settings)  "es-smtp-host"
    setValue (show   $ esSMTPPort settings)  "es-smtp-port"
    setValue (unpack $ esUsername settings)  "es-username"
    setValue (unpack $ esPassword settings)  "es-password"
    setValue (show   $ esSSL settings)       "es-ssl"
    setValue (unpack $ esEmailFrom settings) "es-email-from"
    setValue (unpack $ esEmailTo settings)   "es-email-to"
    setValue (unpack $ esSubject settings)   "es-subject"

  setValue elValue elId =
    unless (null elValue || elValue == "-1") $
      findAndSet (set value elValue) window elId

restoreEventsSettings :: Window -> UI ()
restoreEventsSettings window =
  setEventsSettings =<< liftIO readSavedEventsSettings
 where
  setEventsSettings settings = do
    setState (fst $ evsErrors settings)      "switch-errors"
    setState (fst $ evsCriticals settings)   "switch-criticals"
    setState (fst $ evsAlerts settings)      "switch-alerts"
    setState (fst $ evsEmergencies settings) "switch-emergencies"

    setPeriod (snd $ evsErrors settings)      "select-period-errors"
    setPeriod (snd $ evsCriticals settings)   "select-period-criticals"
    setPeriod (snd $ evsAlerts settings)      "select-period-alerts"
    setPeriod (snd $ evsEmergencies settings) "select-period-emergencies"

  setState elState elId =
    findAndSet (set UI.checked elState) window elId

  setPeriod elPeriod elId =
    selectOption elId $ fromIntegral elPeriod

saveEmailSettings :: Window -> UI ()
saveEmailSettings window =
  (liftIO . saveEmailSettingsOnDisk) =<< getCurrentEmailSettings window

saveEventsSettings :: Window -> UI ()
saveEventsSettings window =
  (liftIO . saveEventsSettingsOnDisk) =<< getCurrentEventsSettings window

getCurrentEmailSettings :: Window -> UI EmailSettings
getCurrentEmailSettings window = do
  smtpHost  <- findAndGetValue window "es-smtp-host"
  smtpPort  <- findAndGetValue window "es-smtp-port"
  username  <- findAndGetValue window "es-username"
  password  <- findAndGetValue window "es-password"
  ssl       <- findAndGetValue window "es-ssl"
  emailFrom <- findAndGetValue window "es-email-from"
  emailTo   <- findAndGetValue window "es-email-to"
  subject   <- findAndGetValue window "es-subject"
  return $ EmailSettings
    { esSMTPHost  = pack smtpHost
    , esSMTPPort  = readInt (pack smtpPort) (-1)
    , esUsername  = pack username
    , esPassword  = pack password
    , esSSL       = read ssl
    , esEmailFrom = pack emailFrom
    , esEmailTo   = pack emailTo
    , esSubject   = pack subject
    }

getCurrentEventsSettings :: Window -> UI EventsSettings
getCurrentEventsSettings window = do
  errorsState      <- fromMaybe False <$> findAndGetCheckboxState window "switch-errors"
  criticalsState   <- fromMaybe False <$> findAndGetCheckboxState window "switch-criticals"
  alertsState      <- fromMaybe False <$> findAndGetCheckboxState window "switch-alerts"
  emergenciesState <- fromMaybe False <$> findAndGetCheckboxState window "switch-emergencies"

  errorsPeriod      <- readPeriod 1800 <$> findAndGetValue window "select-period-errors"
  criticalsPeriod   <- readPeriod 1800 <$> findAndGetValue window "select-period-criticals"
  alertsPeriod      <- readPeriod 1800 <$> findAndGetValue window "select-period-alerts"
  emergenciesPeriod <- readPeriod 1800 <$> findAndGetValue window "select-period-emergencies"

  return $ EventsSettings
    { evsErrors      = (errorsState,      errorsPeriod)
    , evsCriticals   = (criticalsState,   criticalsPeriod)
    , evsAlerts      = (alertsState,      alertsPeriod)
    , evsEmergencies = (emergenciesState, emergenciesPeriod)
    }
 where
  readPeriod :: PeriodInSec -> String -> PeriodInSec
  readPeriod defP s =
    case decimal (pack s) of
      Left _ -> defP
      Right ((i :: Int), _) -> fromIntegral i

setStatusTestEmailButton :: Window -> UI ()
setStatusTestEmailButton window = do
  EmailSettings host _ user pass _ eFrom eTo _ <- getCurrentEmailSettings window
  let allRequiredIsHere =
           isHere host
        && isHere user
        && isHere pass
        && isHere eFrom
        && isHere eTo
  findAndSet (set UI.enabled allRequiredIsHere) window "send-test-email"
 where
  isHere = not . T.null
