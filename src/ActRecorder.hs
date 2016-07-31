{-|
Module      : ActRecorder
Description : ActRecorder's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module ActRecorder
    ( listenSession
    ) where

import ActRecorder.Prelude
import ActRecorder.Config
import qualified Database.PostgreSQL.LibPQ as PQ

listenSession :: AppConfig -> (ByteString -> IO ()) -> IO ()
listenSession conf withNotification = do
  pqCon <- PQ.connectdb $ toS pgSettings
  listen pqCon
  waitForNotifications pqCon
  where
    waitForNotifications = forever . fetch
    listen con = void $ PQ.exec con "LISTEN frontend"
    pgSettings = configDatabase conf
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
