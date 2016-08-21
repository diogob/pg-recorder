module Main where

import           ActRecorder
import           ActRecorder.Config
import           Protolude

import           System.IO                 (BufferMode (..), hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  conf <- readOptions
  notificationHandler <- dbNotificationHandler conf
  putStrLn $ "Listening for notification on " <> configDatabase conf <> " channel: " <> channel conf
  listenSession conf notificationHandler
