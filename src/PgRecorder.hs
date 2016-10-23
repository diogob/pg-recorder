{-|
Module      : PgRecorder
Description : PgRecorder's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module PgRecorder
    ( listenSession
    , dbNotificationHandler
    , createExecutorsPool
    ) where

import PgRecorder.Prelude
import PgRecorder.Config
import Data.Pool (Pool (..), createPool, withResource)
import qualified Database.PostgreSQL.LibPQ as PQ

-- Given a Notification in the form of ByteString we take some IO action
type NotificationHandler = ByteString -> IO()

-- | Given a set of configurations and a way to handle notifications we loop forever fetching notifications and triggering the handler
listenSession :: AppConfig -> NotificationHandler -> IO ()
listenSession conf withNotification = do
  pqCon <- PQ.connectdb pgSettings
  listen pqCon
  waitForNotifications pqCon
  where
    waitForNotifications = forever . fetch
    listen con = void $ PQ.exec con ("LISTEN " <> listenChannel)
    pgSettings = toS $ configDatabase conf
    listenChannel = toS $ channel conf
    fetch con = do
      mNotification <- PQ.notifies con
      case mNotification of
        Nothing -> do
          mfd <- PQ.socket con
          case mfd of
            Nothing  -> panic "Error checking for PostgreSQL notifications"
            Just fd -> do
              (waitRead, _) <- threadWaitReadSTM fd
              atomically waitRead
              void $ PQ.consumeInput con
        Just notification ->
          withNotification $ PQ.notifyExtra notification

-- | Given a set of configurations creates a database connection pool and returns an IO database dispatcher to handle notifications
dbNotificationHandler :: AppConfig -> IO NotificationHandler
dbNotificationHandler conf = do
  pool <- createExecutorsPool conf
  return $ dispatchNotificationToDb pool listenChannel
  where
    listenChannel = toS $ channel conf

-- | Given a set of configurations creates a database connection pool to be used in handlers that need the database
createExecutorsPool :: AppConfig -> IO (Pool PQ.Connection)
createExecutorsPool conf =
  createPool createResource destroyResource resourceStripes ttlInSeconds size
  where
    createResource = PQ.connectdb $ toS $ configDatabase conf
    destroyResource = PQ.finish
    resourceStripes = 1
    ttlInSeconds = 10
    size = 10

-- private functions

-- | Given a pool of database connections and a handler dispatches the handler to be executed in its own thread
dispatchNotificationToDb :: Pool PQ.Connection -> ByteString -> NotificationHandler
dispatchNotificationToDb pool listenChannel notification = void $ forkIO executeNotification
  where
    executeNotification = withResource pool callProcedure
    callProcedure con = void $ PQ.exec con ("SELECT " <> listenChannel <> "('" <> notification <> "')")
