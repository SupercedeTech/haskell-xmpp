
-- | see if we can run any test at all
module RoomSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "connect to server" $ it "gets a connection" $ do
    handle <- liftIO $ connectViaTcp "localhost" 5222
    (result, stream) <- runXmppMonad $ initStream handle "localhost" "jappie" "pass" "someResource"
    nodeResource <- either (throwIO . show) pure result
    runXmppMonad' stream closeStream
  describe "connect to room" $ it "works" $ 1 `shouldBe` 1
  describe "exchange messages" $ it "works" $ 1 `shouldBe` 1
