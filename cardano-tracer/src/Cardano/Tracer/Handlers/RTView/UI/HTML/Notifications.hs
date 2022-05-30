{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Notifications
  ( mkNotificationsEvents
  , mkNotificationsSettings
  ) where

import           Control.Monad (void)
import           Control.Monad.Extra (whenJustM)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Text.Read (readMaybe)

import           Cardano.Tracer.Handlers.RTView.Notifications.Email
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.Notifications.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Notifications
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkNotificationsEvents :: EventsQueues -> UI Element
mkNotificationsEvents eventsQueues = do
  closeIt <- UI.button #. "delete"

  switchErrors      <- mkSwitch "switch-errors"      "Errors"      "danger"
  switchCriticals   <- mkSwitch "switch-criticals"   "Criticals"   "danger"
  switchAlerts      <- mkSwitch "switch-alerts"      "Alerts"      "danger"
  switchEmergencies <- mkSwitch "switch-emergencies" "Emergencies" "danger"
  switchMissedSlots <- mkSwitch "switch-missed-slots" "Missed slots" "info"

  selectNotifyPeriodErrors      <- mkSelect
  selectNotifyPeriodCriticals   <- mkSelect
  selectNotifyPeriodAlerts      <- mkSelect
  selectNotifyPeriodEmergencies <- mkSelect
  selectNotifyPeriodMissed      <- mkSelect

  notifications <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card" #+
          [ UI.header #. "modal-card-head rt-view-notifications-head" #+
              [ UI.p #. "modal-card-title rt-view-notifications-title" # set text "Notifications: events"
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-notifications-body" #+
              [ mkDivider "Common Errors"
              , UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ element switchErrors
                      , element switchCriticals
                      , element switchAlerts
                      , element switchEmergencies
                      ]
                  , UI.div #. "column" #+
                      [ mkErrorsSelectWrapper selectNotifyPeriodErrors
                      , mkErrorsSelectWrapper selectNotifyPeriodCriticals
                      , mkErrorsSelectWrapper selectNotifyPeriodAlerts
                      , mkErrorsSelectWrapper selectNotifyPeriodEmergencies
                      ]
                  ]
              , mkDivider "Blockchain"
              , UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ element switchMissedSlots
                      ]
                  , UI.div #. "column" #+
                      [ mkErrorsSelectWrapper selectNotifyPeriodMissed
                      ]
                  ]
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-notification-settings-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field is-grouped" #+
                          [ UI.p #. "control" #+
                              [ string "TEST"
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      ]

  on UI.click closeIt . const $ element notifications #. "modal"

  on UI.checkedChange switchErrors $
    liftIO . updateNotificationsEvents eventsQueues EventErrors
  on UI.checkedChange switchCriticals $
    liftIO . updateNotificationsEvents eventsQueues EventCriticals
  on UI.checkedChange switchAlerts $
    liftIO . updateNotificationsEvents eventsQueues EventAlerts
  on UI.checkedChange switchEmergencies $
    liftIO . updateNotificationsEvents eventsQueues EventEmergencies

  handleSelectChange selectNotifyPeriodErrors      EventErrors
  handleSelectChange selectNotifyPeriodCriticals   EventCriticals
  handleSelectChange selectNotifyPeriodAlerts      EventAlerts
  handleSelectChange selectNotifyPeriodEmergencies EventEmergencies

  return notifications
 where
  handleSelectChange selector eventGroup =
    on UI.selectionChange selector . const $
      whenJustM (readMaybe <$> get value selector) $ \(period :: PeriodInSec) ->
        liftIO $ updateNotificationsPeriods eventsQueues eventGroup period

mkDivider :: String -> UI Element
mkDivider dTitle =
  UI.div #. "divider rt-view-divider" #+
    [ UI.span #. "rt-view-events-title" # set text dTitle
    ]

mkErrorsSelectWrapper :: Element -> UI Element
mkErrorsSelectWrapper sel =
  UI.div #. "rt-view-notifications-errors-select-wrapper" #+
    [ UI.div #. "select is-link is-small" #+ [element sel]
    ]

mkSelect :: UI Element
mkSelect =
  UI.select #+
    -- Values are periods in seconds.
    [ UI.option # set value "5"     # set text "Immediately"
    , UI.option # set value "300"   # set text "Every 5 minutes"
    , UI.option # set value "1800"  # set text "Every 30 minutes"
    , UI.option # set value "3600"  # set text "Every 1 hour"
    , UI.option # set value "10800" # set text "Every 3 hours"
    , UI.option # set value "21600" # set text "Every 6 hours"
    ]

mkSwitch
  :: String
  -> String
  -> String
  -> UI Element
mkSwitch switchId switchName bulmaColorName =
  UI.div #. "field" #+
    [ UI.input ## switchId
               #. ("switch is-rounded is-" <> bulmaColorName)
               # set UI.type_ "checkbox"
               # set UI.name switchId
    , UI.label # set UI.for switchId
               # set text switchName
    ]

-- | Settings for notifications (email, etc.).

mkNotificationsSettings :: UI Element
mkNotificationsSettings = do
  window <- askWindow
  closeIt <- UI.button #. "delete"
  sendTestEmail <- UI.button ## "send-test-email"
                             #. "button is-primary"
                             # set text "Send test email"
  sendTestEmailStatus <- UI.span # set text ""
  showHidePasswordIcon <- image "rt-view-show-hide-pass-icon" hideSVG
  showHidePassword <- UI.button #. "button is-info"
                                # set dataState hiddenState
                                #+ [element showHidePasswordIcon]
  inputHost <- UI.input ## "es-smtp-host"
                        #. "input is-normal"
                        # set (attr "placeholder") "e.g. smtp.gmail.com"
                        # set (attr "required") "required"
  inputUser <- UI.input ## "es-username"
                        #. "input is-normal"
                        # set (attr "placeholder") "e.g. your.name@gmail.com"
                        # set (attr "required") "required"
  inputPassword <- UI.input ## "es-password"
                            #. "input is-normal"
                            # set UI.type_ "password"
                            # set (attr "placeholder") "your password"
                            # set (attr "required") "required"
  inputEmailFrom <- UI.input ## "es-email-from"
                             #. "input is-normal"
                             # set UI.type_ "email"
                             # set (attr "placeholder") "e.g. your.no.reply@gmail.com"
                             # set (attr "required") "required"
  inputEmailTo <- UI.input ## "es-email-to"
                           #. "input is-normal"
                           # set UI.type_ "email"
                           # set (attr "placeholder") "e.g. your.name@gmail.com"
                           # set (attr "required") "required"
  notifications <-
    UI.div #. "modal" #+
      [ UI.div #. "modal-background" #+ []
      , UI.div #. "modal-card rt-view-notifications-settings" #+
          [ UI.header #. "modal-card-head rt-view-notifications-head" #+
              [ UI.p #. "modal-card-title rt-view-notifications-title" # set text "Notifications: settings"
              , element closeIt
              ]
          , UI.mkElement "section" #. "modal-card-body rt-view-notifications-body" #+
              [ UI.p #. "rt-view-email-only" #+
                  [ string "Currently, only email notifications are supported"
                  ]
              , mkControlPair "SMTP host *" $ element inputHost
              , mkControlPair "SMTP port" $
                  UI.div #. "select" #+
                    [ UI.select ## "es-smtp-port" #+
                        [ UI.option # set value "25"   # set text "25"
                        , UI.option # set value "465"  # set text "465"
                        , UI.option # set value "587"  # set text "587"
                        , UI.option # set value "2525" # set text "2525"
                        ]
                    ]
              , mkControlPair "Username *" $ element inputUser
              , UI.div #. "field is-horizontal" #+
                  [ UI.div #. "field-label is-normal" #+
                      [ UI.label #. "label rt-view-label" # set text "Password *"
                      ]
                  , UI.div #. "field-body" #+
                      [ UI.div #. "field has-addons" #+
                          [ UI.p #. "control" #+
                              [ element inputPassword
                              ]
                          , UI.div #. "control" #+
                              [ element showHidePassword
                              ]
                          ]
                      ]
                  ]
              , mkControlPair "SSL" $
                  UI.div #. "select" #+
                    [ UI.select ## "es-ssl" #+
                        [ UI.option # set value (show TLS)      # set text "TLS"
                        , UI.option # set value (show STARTTLS) # set text "STARTTLS"
                        , UI.option # set value (show NoSSL)    # set text "No SSL"
                        ]
                    ]
              , mkControlPair "From *" $ element inputEmailFrom
              , mkControlPair "To *" $ element inputEmailTo
              , mkControlPair "Subject" $
                  UI.input ## "es-subject"
                           #. "input is-normal"
                           # set (attr "placeholder") "e.g. Cardano RTView Notification"
              ]
          , UI.mkElement "footer" #. "modal-card-foot rt-view-notification-settings-foot" #+
              [ UI.div #. "columns" #+
                  [ UI.div #. "column" #+
                      [ UI.div #. "field is-grouped" #+
                          [ UI.p #. "control" #+
                              [ element sendTestEmail
                              ]
                          , UI.p #. "control" #+
                              [ element sendTestEmailStatus
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      ]

  on UI.click closeIt . const $ do
    void $ element notifications #. "modal"
    void $ element sendTestEmailStatus # set text ""
    saveEmailSettings window

  on UI.click sendTestEmail . const $ do
    void $ element sendTestEmail #. "button is-primary is-loading"
                                 # set UI.enabled False
    statusMessage <- liftIO . createAndSendTestEmail =<< getCurrentEmailSettings window
    void $ element sendTestEmailStatus # set text (T.unpack statusMessage)
    void $ element sendTestEmail #. "button is-primary"
                                 # set UI.enabled True

  on UI.click showHidePassword . const $ do
    state <- get dataState showHidePassword
    let haveToHide = state == shownState
    if haveToHide
      then do
        void $ element showHidePasswordIcon # set html hideSVG
        void $ element showHidePassword # set dataState hiddenState
        void $ element inputPassword # set UI.type_ "password"
      else do
        void $ element showHidePasswordIcon # set html showSVG
        void $ element showHidePassword # set dataState shownState
        void $ element inputPassword # set UI.type_ "text"

  on UI.valueChange inputHost . const      $ setStatusTestEmailButton window
  on UI.valueChange inputUser . const      $ setStatusTestEmailButton window
  on UI.valueChange inputPassword . const  $ setStatusTestEmailButton window
  on UI.valueChange inputEmailFrom . const $ setStatusTestEmailButton window
  on UI.valueChange inputEmailTo . const   $ setStatusTestEmailButton window

  return notifications

mkControlPair
  :: String
  -> UI Element
  -> UI Element
mkControlPair labelText control =
  UI.div #. "field is-horizontal" #+
    [ UI.div #. "field-label is-normal" #+
        [ UI.label #. "label rt-view-label" # set text labelText
        ]
    , UI.div #. "field-body" #+
        [ UI.div #. "field" #+
            [ UI.p #. "control" #+
                [ control
                ]
            ]
        ]
    ]
