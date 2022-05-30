{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Email
  ( StatusMessage
  , createAndSendEmail
  , createAndSendTestEmail
  ) where

import           Control.Exception.Extra (try_)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Network.Mail.Mime (Address (..), Mail (..), simpleMail')
import qualified Network.Mail.SMTP as SMTP

import           Cardano.Tracer.Handlers.RTView.Notifications.Types

type StatusMessage = Text

createAndSendEmail
  :: EmailSettings
  -> Text
  -> IO StatusMessage
createAndSendEmail settings@EmailSettings {esEmailTo, esEmailFrom, esSubject} bodyMessage =
  sendEmail settings $ simpleMail' to from esSubject body
 where
  to   = Address Nothing esEmailTo
  from = Address (Just "Cardano RTView") esEmailFrom
  body = LT.fromStrict bodyMessage

createAndSendTestEmail
  :: EmailSettings
  -> IO StatusMessage
createAndSendTestEmail settings = createAndSendEmail settings body
 where
  body = "This is a test notification from Cardano RTView. Congrats: your email settings are correct!"

sendEmail
  :: EmailSettings
  -> Mail
  -> IO StatusMessage
sendEmail EmailSettings {esSMTPHost, esSMTPPort, esUsername, esPassword, esSSL} mail =
  try_ (sender host port user pass mail) >>= \case
    Left e  -> return $ "Unable to send email: " <> T.pack (show e)
    Right _ -> return "Yay! Notification is sent."
 where
  sender = case esSSL of
             TLS      -> SMTP.sendMailWithLoginTLS'
             STARTTLS -> SMTP.sendMailWithLoginSTARTTLS'
             NoSSL    -> SMTP.sendMailWithLogin'
  host = T.unpack esSMTPHost
  port = fromIntegral esSMTPPort
  user = T.unpack esUsername
  pass = T.unpack esPassword
