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
( createRoomStanza, leaveRoomStanza, destroyRoomStanza
, roomMessageStanza, privateMessageStanza, queryInstantRoomConfigStanza
, queryForAssociatedServicesStanza, submitInstantRoomConfigStanza
, setRoomMembersListStanza
, UserJID, RoomJID, RoomMemberJID, FromXML(..), MUCPayload(..), RoomMembersList(..)
, Affiliation(..)
)
where

import qualified Data.UUID          as UUID
import qualified Data.Text          as T
import           Data.Maybe         (listToMaybe)
import           Data.Time          (UTCTime)
import           Text.Hamlet.XML    (xml)
import           Text.XML.HaXml.Xtract.Parse (xtract)

import           Network.XMPP.Types
import           Network.XMPP.XML
import           Network.XMPP.Stanza
import           Network.XMPP.XEP.Form

type UserJID = JID 'NodeResource       -- fully qualified user JID in Jabber: for example - JohnWick@localhost/riskbook-web
type RoomJID = JID 'Node               -- for example - programmers@localhost
type RoomMemberJID = JID 'NodeResource -- for example - programmers@localhost/NikitaRzm

-- | https://xmpp.org/extensions/xep-0045.html#disco-service
queryForAssociatedServicesStanza :: JID 'NodeResource -> Server -> UUID.UUID -> Stanza 'IQ 'Outgoing MUCPayload
queryForAssociatedServicesStanza from srv uuid =
  MkIQ
    { iqFrom = Just $ SomeJID from
    , iqTo   = Just $ SomeJID $ DomainJID $ DomainID srv
    , iqId   = UUID.toText uuid
    , iqType = Get
    , iqBody = [xml|<query xmlns='http://jabber.org/protocol/disco#items'/>|]
    , iqPurpose = SOutgoing
    }

createRoomStanza :: UserJID -> UserJID -> UUID.UUID -> Stanza 'Presence 'Outgoing MUCPayload
createRoomStanza who room uuid =
  MkPresence
    { pFrom     = Just $ SomeJID who
    , pTo       = Just $ SomeJID room
    , pId       = UUID.toText uuid
    , pType     = Default
    , pShowType = Available
    , pStatus   = ""
    , pPriority = Nothing
    , pExt     = [xml|<x xmlns="http://jabber.org/protocol/muc">|]
    , pPurpose = SOutgoing
    }

leaveRoomStanza :: JID 'NodeResource -> UUID.UUID -> Stanza 'Presence 'Outgoing MUCPayload
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

destroyRoomStanza :: UserJID -> RoomJID -> T.Text -> UUID.UUID -> Stanza 'IQ 'Outgoing ()
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
  -> Stanza 'Message 'Outgoing ()
privateMessageStanza from to msg uuid = 
  MkMessage
    { mFrom    = Just $ SomeJID from
    , mTo      = Just $ SomeJID to
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
  -> Stanza 'Message 'Outgoing ()
roomMessageStanza from to msg uuid =
  MkMessage
    { mFrom    = Just $ SomeJID from
    , mTo      = Just $ SomeJID to
    , mId      = UUID.toText uuid
    , mType    = GroupChat
    , mSubject = ""
    , mBody    = msg
    , mThread  = ""
    , mExt     = []
    , mPurpose = SOutgoing
    }

queryInstantRoomConfigStanza :: UserJID -> RoomJID -> UUID.UUID -> Stanza 'IQ 'Outgoing ()
queryInstantRoomConfigStanza owner room uuid = 
  MkIQ
    { iqFrom = Just $ SomeJID owner
    , iqTo   = Just $ SomeJID room
    , iqId   = UUID.toText uuid
    , iqType = Get
    , iqBody = [xml| <query xmlns="http://jabber.org/protocol/muc#owner"> |]
    , iqPurpose = SOutgoing
    }

submitInstantRoomConfigStanza :: UserJID -> RoomJID -> XmppForm -> UUID.UUID -> Stanza 'IQ 'Outgoing ()
submitInstantRoomConfigStanza owner room form uuid =
  MkIQ
    { iqFrom = Just $ SomeJID owner
    , iqTo   = Just $ SomeJID room
    , iqId   = UUID.toText uuid
    , iqType = Set
    , iqBody = [xml|<query xmlns="http://jabber.org/protocol/muc#owner">^{encodeXml form}|]
    , iqPurpose = SOutgoing
    }

setRoomMembersListStanza :: RoomJID -> UserJID -> RoomMembersList -> UUID.UUID -> Stanza 'IQ 'Outgoing ()
setRoomMembersListStanza room admin members uuid =
  MkIQ
    { iqFrom = Just $ SomeJID admin
    , iqTo   = Just $ SomeJID room
    , iqId   = UUID.toText uuid
    , iqType = Set
    , iqBody = [xml|
        <query xmlns="http://jabber.org/protocol/muc#admin">
          ^{encodeXml members}
        |]
    , iqPurpose = SOutgoing
    }

data Affiliation =
    OwnerAffiliation
  | AdminAffiliation
  | MemberAffiliation
  | OutcastAffiliation
  | NoneAffiliation
  deriving (Eq, Show)

data Role =
    ModeratorRole
  | NoneRole
  | ParticipantRole
  | VisitorRole
  deriving (Eq, Show)

data MUCPayload =
    MUCRoomCreated Affiliation Role
  | MUCRoomQuery XmppForm
  | MUCRoomConfigRejected
  | MUCMembersPresences Affiliation Role
  | MUCMessageId T.Text
  | MUCArchivedMessage
    { mamMessage  :: Stanza 'Message 'Incoming ()
    , mamFrom     :: JID 'Domain
    , mamWhen     :: UTCTime
    , mamStoredId :: T.Text
    }
  deriving (Show)

newtype RoomMembersList = RoomMembersList [(UserJID, Affiliation)]
  deriving (Eq, Show)

instance ToXML RoomMembersList where
  encodeXml (RoomMembersList members) =
    [xml|
      $forall (jid, affiliation) <- members
        <item affiliation="#{encodeAffiliation affiliation}"
              jid="#{T.pack $ show $ toBareJID jid}">
    |]

instance FromXML MUCPayload where
  decodeXml m
    | matchPatterns m ["/x/item/@jid", "/x/item/@role", "/x/item/@affiliation"]
    = MUCRoomCreated
      <$> parseAffiliation (txtpat "/x/item/@affiliation" m)
      <*> parseRole (txtpat "/x/item/@role" m)
    | matchPatterns m ["/query/x"]
    = MUCRoomQuery <$> (listToMaybe (xtract id "/query/x" m) >>= decodeXml)
    | matchPatterns
      m
      [ "/query/[@type='cancel]"
      , "/query/[@xmlns='http://jabber.org/protocol/muc#owner']"
      ]
    = Just MUCRoomConfigRejected
    | matchPatterns m ["/x/item/@affiliation", "/x/item/@role"]
    = MUCMembersPresences
      <$> parseAffiliation (txtpat "/x/item/@affiliation" m)
      <*> parseRole (txtpat "/x/item/@role" m)
    | matchPatterns m ["/result", "/result/forwarded/message"]
    = 
      let mMsg = listToMaybe (xtract id "/result/forwarded/message" m) >>= decodeStanza
          mFrom = mread $ txtpat "/result/forwarded/delay/@from" m
          mTime = mread $ T.replace "T" " " $ txtpat "/result/forwarded/delay/@stamp" m
          storedId = txtpat "/result/forwarded/message/stanza-id/@id" m
      in MUCArchivedMessage <$> mMsg <*> mFrom <*> mTime <*> Just storedId
    | matchPatterns m ["/stanza-id/@id"] = Just $ MUCMessageId $ txtpat "/stanza-id/@id" m
    | otherwise = Nothing

encodeAffiliation :: Affiliation -> T.Text
encodeAffiliation OwnerAffiliation   = "owner"
encodeAffiliation AdminAffiliation   = "admin"
encodeAffiliation MemberAffiliation  = "member"
encodeAffiliation OutcastAffiliation = "outcast"
encodeAffiliation NoneAffiliation    = "none"

parseAffiliation :: T.Text -> Maybe Affiliation
parseAffiliation v = case v of
      "owner"   -> Just OwnerAffiliation
      "admin"   -> Just AdminAffiliation
      "member"  -> Just MemberAffiliation
      "outcast" -> Just OutcastAffiliation
      _         -> Nothing

parseRole ::  T.Text -> Maybe Role
parseRole v = case v of
    "moderator"   -> Just ModeratorRole
    "participant" -> Just ParticipantRole
    "visitor"     -> Just VisitorRole
    _             -> Nothing
