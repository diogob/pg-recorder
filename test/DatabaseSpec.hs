module DatabaseSpec (spec) where

import Protolude

import           Data.Function                        (id)
import qualified Hasql.Connection                     as H
import qualified Hasql.Pool                           as HP
import Test.Hspec

import PgRecorder.Database

spec :: Spec
spec =
  describe "waitForNotifications" $
    it "does not block the connection ann trigger action upon notification" $ do
      conOrError <- H.acquire "postgres://localhost/postgrest_test"
      let con = either (panic . show) id conOrError :: H.Connection
      notification <- liftIO newEmptyMVar

      void $ forkIO $ waitForNotifications (curry $ putMVar notification) con
      listen con $ toPgIdentifier "test"

      pool <- HP.acquire (1, 1, "postgres://localhost/postgrest_test")
      void $ either (panic "error calling procedure") id <$> callProcedure pool (toPgIdentifier "pg_notify") "test" "hello there"

      readMVar notification `shouldReturn` ("test", "hello there")
