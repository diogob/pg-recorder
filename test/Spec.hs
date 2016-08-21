import Protolude
import ActRecorder
import ActRecorder.Config
import Data.Pool (withResource)
import qualified Database.PostgreSQL.LibPQ as PQ
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "ActRecorder.createPool" $ do
      it "should connect and allow operations with pool" $ do
        pool <- createPool pgSettings
        void $ withResource pool $ flip PQ.exec "LISTEN frontend"
  where
    pgSettings = AppConfig "postgres://localhost/recorder_test" "test"
