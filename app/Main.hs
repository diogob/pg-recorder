module Main where

import           PgRecorder
import           PgRecorder.Config
import           Protolude

import           System.IO                 (BufferMode (..), hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  conf <- readOptions
  let poolSettings = (10, 10, toS $ configDatabase conf)
      dispatcher = dispatcherFunction conf
  notificationHandler <- dbNotificationHandler poolSettings dispatcher
  putStrLn $ "Listening for notification on " <> configDatabase conf <> " using dispatcher: " <> dispatcher
  listenSession conf notificationHandler
