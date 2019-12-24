{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.XEP.MUC
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- XEP-0045, join\/kick\/ban\/leave functionality
--
-----------------------------------------------------------------------------
module Network.XMPP.XEP.MUC
( enterRoomStanza, leaveRoomStanza, destroyRoomStanza
, roomMessageStanza, privateMessageStanza
, queryForAssociatedServicesStanza
, UserJID, RoomJID, RoomMemberJID
) 
where

import qualified Data.UUID          as UUID
import qualified Data.Text          as T
import           Text.Hamlet.XML    (xml)
import           Network.XMPP.Types


type UserJID = JID 'NodeResource       -- fully qualified user JID in Jabber: for example - JohnWick@localhost/riskbook-web
type RoomJID = JID 'Node               -- for example - programmers@localhost
type RoomMemberJID = JID 'NodeResource -- for example - programmers@localhost/NikitaRzm

-- | https://xmpp.org/extensions/xep-0045.html#disco-service
queryForAssociatedServicesStanza :: JID 'NodeResource -> Server -> UUID.UUID -> Stanza 'IQ 'Outgoing
queryForAssociatedServicesStanza from srv uuid =
  MkIQ
    { iqFrom = Just $ SomeJID from
    , iqTo   = Just $ SomeJID $ DomainJID $ DomainID srv
    , iqId   = UUID.toText uuid
    , iqType = Get
    , iqBody = [xml|<query xmlns='http://jabber.org/protocol/disco#items'/>|]
    , iqPurpose = SOutgoing
    }

enterRoomStanza :: UserJID -> UserJID -> UUID.UUID -> Stanza 'Presence 'Outgoing
enterRoomStanza who to uuid =
  MkPresence
    { pFrom     = Just $ SomeJID who
    , pTo       = Just $ SomeJID to
    , pId       = UUID.toText uuid
    , pType     = Default
    , pShowType = Available
    , pStatus   = ""
    , pPriority = Nothing
    , pExt     = [xml|<x xmlns="http://jabber.org/protocol/muc">|]
    , pPurpose = SOutgoing
    }

leaveRoomStanza :: JID 'NodeResource -> UUID.UUID -> Stanza 'Presence 'Outgoing
leaveRoomStanza jid uuid =
  MkPresence
    { pFrom     = Nothing
    , pTo       = Just $ SomeJID jid
    , pId       = UUID.toText uuid
    , pType     = Unavailable
    , pShowType = Available
    , pStatus   = ""
    , pPriority = Nothing
    , pExt   = []
    , pPurpose = SOutgoing
    }

destroyRoomStanza :: UserJID -> RoomJID -> T.Text -> UUID.UUID -> Stanza 'IQ 'Outgoing
destroyRoomStanza owner room reason uuid =
  MkIQ
    { iqFrom = Just $ SomeJID owner
    , iqTo   = Just $ SomeJID room
    , iqId   = UUID.toText uuid
    , iqType = Set
    , iqBody = [xml|
        <query xmlns="http://jabber.org/protocol/muc#owner">
          <destroy jid="#{T.pack (show room)}">
            <reason>#{reason}</reason>
        |]
    , iqPurpose = SOutgoing
    }

privateMessageStanza
  :: UserJID
  -> RoomMemberJID
  -> T.Text
  -> UUID.UUID
  -> Stanza 'Message 'Outgoing
privateMessageStanza from to msg uuid = 
  MkMessage
    { mFrom    = Just from
    , mTo      = SomeJID to
    , mId      = UUID.toText uuid
    , mType    = Chat
    , mSubject = ""
    , mBody    = msg
    , mThread  = ""
    , mExt     = []
    , mPurpose = SOutgoing
    }

roomMessageStanza
  :: UserJID
  -> RoomJID
  -> T.Text
  -> UUID.UUID
  -> Stanza 'Message 'Outgoing
roomMessageStanza from to msg uuid =
  MkMessage
    { mFrom    = Just from
    , mTo      = SomeJID to
    , mId      = UUID.toText uuid
    , mType    = GroupChat
    , mSubject = ""
    , mBody    = msg
    , mThread  = ""
    , mExt     = []
    , mPurpose = SOutgoing
    }
