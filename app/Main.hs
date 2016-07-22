module Main where

import Protolude
import ActRecorder.Config

main :: IO ()
main = do
  conf <- readOptions
  let dbConfig = configDatabase conf
  putStrLn dbConfig
