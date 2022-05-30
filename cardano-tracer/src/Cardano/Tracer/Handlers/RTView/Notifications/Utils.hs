module Cardano.Tracer.Handlers.RTView.Notifications.Utils
  ( addNewEvent
  , getNewEvents
  , initEventsQueues
  , updateNotificationsEvents
  , updateNotificationsPeriods
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (flushTBQueue, isFullTBQueue, newTBQueueIO,
                   writeTBQueue)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO)
import           Control.Monad.Extra (unlessM, whenJust)
import qualified Data.Map.Strict as M

import           Cardano.Tracer.Handlers.RTView.Notifications.Send
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Handlers.RTView.Notifications.Types

initEventsQueues :: IO EventsQueues
initEventsQueues = do
  errsQ <- initEventsQueue
  critQ <- initEventsQueue
  alrtQ <- initEventsQueue
  emrgQ <- initEventsQueue

  errsT <- mkTimer (makeAndSendNotification errsQ) initCallPeriod
  critT <- mkTimer (makeAndSendNotification critQ) initCallPeriod
  alrtT <- mkTimer (makeAndSendNotification alrtQ) initCallPeriod
  emrgT <- mkTimer (makeAndSendNotification emrgQ) initCallPeriod

  newTVarIO $ M.fromList
    [ (EventErrors,      (errsQ, errsT))
    , (EventCriticals,   (critQ, critT))
    , (EventAlerts,      (alrtQ, alrtT))
    , (EventEmergencies, (emrgQ, emrgT))
    ]
 where
  initEventsQueue = newTBQueueIO 2000
  initCallPeriod = 60

getNewEvents
  :: EventsQueues
  -> EventGroup
  -> IO [Event]
getNewEvents eventsQueues eventGroup = do
  queues <- readTVarIO eventsQueues
  case M.lookup eventGroup queues of
    Nothing -> return []
    Just (queue, _) -> atomically $ flushTBQueue queue

addNewEvent
  :: EventsQueue
  -> Event
  -> IO ()
addNewEvent eventsQueue event = atomically $
  unlessM (isFullTBQueue eventsQueue) $
    writeTBQueue eventsQueue event

-- | ..
updateNotificationsEvents
  :: EventsQueues
  -> EventGroup
  -> Bool
  -> IO ()
updateNotificationsEvents queues group True  = changeTimerState startTimer queues group
updateNotificationsEvents queues group False = changeTimerState stopTimer  queues group

updateNotificationsPeriods
  :: EventsQueues
  -> EventGroup
  -> PeriodInSec
  -> IO ()
updateNotificationsPeriods queues group period =
  changeTimerState (`setCallPeriod` period) queues group

changeTimerState
  :: (Timer -> IO ())
  -> EventsQueues
  -> EventGroup
  -> IO ()
changeTimerState setter eventsQueues eventGroup = do
  queues <- readTVarIO eventsQueues
  whenJust (M.lookup eventGroup queues) $ setter . snd
