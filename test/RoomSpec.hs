{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | basic stuff like connecting to a room and sending messages
module RoomSpec where

import Network.XMPP.XML
import Network.XMPP.XEP.MUC
import Data.Text(Text)
import Control.Exception
import Test.Hspec
import Network.XMPP.Types
import Network.XMPP.Helpers
import Network.XMPP.Core
import Control.Monad.IO.Class
import Control.Monad
import Network.XMPP.Stream
import Network.XMPP.Ejabberd
import Network.XMPP.Types
import Network.XMPP.Concurrent

deriving instance Exception XmppError

spec :: Spec
spec = describe "ejabbered server tests" $ do
  it "gets a connection" $ do
    handle <- liftIO $ connectViaTcp "localhost" 5222
    registerNewUser localEjabberdHost (EUser "jappie" "pass") (VHost "localhost")
    (result, stream) <- runXmppMonad $ initStream handle "localhost" "jappie" "pass" "someResource"
    nodeResource <- either throwIO pure result
    void $ runXmppMonad' stream closeStream

  it "can connect to a room" $ do
    handle <- liftIO $ connectViaTcp "localhost" 5222
    registerNewUser localEjabberdHost (EUser "jappie" "pass")  (VHost "localhost")
    registerNewUser localEjabberdHost (EUser "jesiska" "pass") (VHost "localhost")
    (result, stream) <- runXmppMonad $ do
      jappie  <- either (error "init failure") id <$> initStream handle "localhost" "jappie"  "pass" "someResource"
      xmppSend =<< withUUID (createRoomStanza jappie (mkParticipantJIDForRoom "jappie" someRoom))
      xmppSend =<< withUUID (createRoomStanza jappie (mkParticipantJIDForRoom "jesiska" someRoom))

    pure $! seq result -- no lazy
    void $ runXmppMonad' stream closeStream

  it "can exchange messages" $ do
    handle <- liftIO $ connectViaTcp "localhost" 5222
    registerNewUser localEjabberdHost (EUser "jappie" "pass")  (VHost "localhost")
    registerNewUser localEjabberdHost (EUser "jesiska" "pass") (VHost "localhost")
    (result, stream) <- runXmppMonad $ do
      jappie  <- either (error "init failure") id <$> initStream handle "localhost" "jappie"  "pass" "someResource"
      xmppSend =<< withUUID (createRoomStanza jappie (mkParticipantJIDForRoom "jappie" someRoom))
      xmppSend =<< withUUID (createRoomStanza jappie (mkParticipantJIDForRoom "jesiska" someRoom ))
      let
          expectedMsg = "some-msg"
      runThreaded @() $ do
        stanza <- withUUID (roomMessageStanza jappie someRoom expectedMsg)
        writeChanS $ SomeStanza stanza
        pure ()

      runThreaded $ do
        stanze <- readChanS
        case stanze of
          Left x -> liftIO $ throwIO x
          Right msg -> case msg :: SomeStanza MUCPayload of
              SomeStanza (MkMessage {mBody,mFrom}) -> do
                case mFrom of
                  Just (SomeJID (resource@NodeResourceJID{})) ->
                    liftIO $ resource `shouldBe` jappie
                  _ -> do
                    error "expecting from"
                    pure ()
                liftIO $ mBody `shouldBe` expectedMsg

              _other -> do
                error $ "expecting message, got something else"
                pure ()
        pure ()

    pure $! seq result -- no lazy
    void $ runXmppMonad' stream closeStream

someRoom :: JID 'Node
someRoom = mkRoomJID "localhost" "blah"

mkRoomJID :: Server -> Text -> JID 'Node
mkRoomJID srv roomId = NodeJID (NodeID roomId) $ DomainID $ "conference." <> srv

mkParticipantJIDForRoom :: Username -> RoomJID -> RoomMemberJID
mkParticipantJIDForRoom username NodeJID {..} =
  NodeResourceJID nNode nDomain $ ResourceID username
