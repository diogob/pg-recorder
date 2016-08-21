{-|
Module      : ActRecorder
Description : ActRecorder's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module ActRecorder
    ( listenSession
    , createExecutorsPool
    ) where

import ActRecorder.Prelude
import ActRecorder.Config
import Data.Pool (Pool (..), createPool)
import qualified Database.PostgreSQL.LibPQ as PQ

createExecutorsPool :: AppConfig -> IO (Pool PQ.Connection)
createExecutorsPool conf =
  createPool createResource destroyResource resourceStripes ttlInSeconds size
  where
    createResource = PQ.connectdb $ toS $ configDatabase conf
    destroyResource = PQ.finish
    resourceStripes = 1
    ttlInSeconds = 10
    size = 10

listenSession :: AppConfig -> (ByteString -> IO ()) -> IO ()
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
