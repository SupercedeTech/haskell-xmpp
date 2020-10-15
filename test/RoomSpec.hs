{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | basic stuff like connecting to a room and sending messages
module RoomSpec where

import Control.Exception
import Test.Hspec
import Network.XMPP.Types
import Network.XMPP.Helpers
import Network.XMPP.Core
import Control.Monad.IO.Class
import Control.Monad
import Network.XMPP.Stream

deriving instance Exception XmppError

spec :: Spec
spec = do
  describe "connect to server" $ it "gets a connection" $ do
    handle <- liftIO $ connectViaTcp "localhost" 5222
    (result, stream) <- runXmppMonad $ initStream handle "localhost" "jappie" "pass" "someResource"
    nodeResource <- either throwIO pure result
    void $ runXmppMonad' stream closeStream
  describe "connect to room" $ it "works" $ 1 `shouldBe` 1
  describe "exchange messages" $ it "works" $ 1 `shouldBe` 1
