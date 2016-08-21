module ActRecorder.Config ( prettyVersion
                          , minimumPgVersion
                          , readOptions
                          , AppConfig (..)
                          ) where

import ActRecorder.Prelude

import qualified Data.Text                   as T
import           Data.Version                (versionBranch)
import           Options.Applicative
import           Options.Applicative.Text
import           Paths_act_recorder          (version)

-- | Data type to store all command line options
data AppConfig = AppConfig { configDatabase  :: Text
                           , channel :: Text
                           }

argParser :: Parser AppConfig
argParser = AppConfig
  <$> argument text (help "(REQUIRED) database connection string, e.g. postgres://user:pass@host:port/db" <> metavar "DB_URL")
  <*> textOption    (long "channel"  <> short 'c' <> help "(REQUIRED) channel to listen to notifications for async commands" <> metavar "CHANNEL")

-- | User friendly version number
prettyVersion :: Text
prettyVersion = T.intercalate "." $ show <$> versionBranch version

-- | Tells the minimum PostgreSQL version required by this version of Haskell Tools
minimumPgVersion :: Integer
minimumPgVersion = 90500

-- | Function to read and parse options from the command line
readOptions :: IO AppConfig
readOptions = customExecParser parserPrefs opts
  where
    opts = info (helper <*> argParser) $
                    fullDesc
                    <> (progDesc . toS) (
                    ("act-recorder " :: Text)
                    <> prettyVersion
                    <> (" / Records session info from Act instances" :: Text)
                    )
    parserPrefs = prefs showHelpOnError
