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
  listenSession conf putStrLn
