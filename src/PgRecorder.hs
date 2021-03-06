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
    ) where

import PgRecorder.Prelude
import PgRecorder.Config
import PgRecorder.Database
import qualified Hasql.Pool as HP
import qualified Hasql.Connection                     as H

-- | Given a set of configurations and a way to handle notifications we loop forever fetching notifications and triggering the handler
listenSession :: AppConfig -> (ByteString -> ByteString -> IO ()) -> IO ()
listenSession conf withNotification = do
  con <- either (panic . show) id <$> H.acquire (toS $ configDatabase conf)
  listen con $ toPgIdentifier . toS $ channel conf
  waitForNotifications withNotification con

-- | Given a set of configurations creates a database connection pool and returns an IO database dispatcher to handle notifications
dbNotificationHandler :: HP.Settings -> Text -> IO (ByteString -> ByteString -> IO ())
dbNotificationHandler poolConfig dispatcher =
  dispatchNotificationToDb (toS dispatcher) <$> HP.acquire poolConfig

-- private functions

-- | Given a pool of database connections and a handler dispatches the handler to be executed in its own thread
dispatchNotificationToDb :: ByteString -> HP.Pool -> ByteString -> ByteString -> IO ()
dispatchNotificationToDb dispatcher pool chan notification =
  void $ async executeNotification
  where
    executeNotification = do
      r <- callProcedure pool (toPgIdentifier dispatcher) chan notification
      case r of
        Left e -> print e
        _ -> return ()
