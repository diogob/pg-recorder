import Protolude
import PgRecorder
import PgRecorder.Config
import Data.Pool (withResource)
import qualified Database.PostgreSQL.LibPQ as PQ
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "PgRecorder.createExecutorsPool" $ do
      it "should connect and allow operations with pool" $ do
        pool <- createExecutorsPool pgSettings
        void $ withResource pool $ flip PQ.exec "LISTEN frontend"
  where
    pgSettings = AppConfig "postgres://localhost/recorder_test" "test"
