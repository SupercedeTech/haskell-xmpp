{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

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
( enterRoom, leaveRoom, destroyRoom
) 
where

import Data.UUID           (UUID, toString)

import Text.Hamlet.XML     (xml)
import Network.XMPP.Types


-- | https://xmpp.org/extensions/xep-0045.html#disco-service
{-
queryForAssociatedServices :: JID a -> Server -> UUID -> Stanza 'IQ
queryForAssociatedServices jid srv uuid =
    MkIQ
        { iqFrom = Just from
        , iqTo   = Just (JID Nothing srv Nothing)
        , iqId   = toString uuid
        , iqType = Get
        , iqBody = [xml|
                      <query xmlns='http://jabber.org/protocol/disco#items'/>
                   |]
        }
        -}

enterRoom :: JID 'NodeResource -> UUID -> Stanza 'Presence 'Outgoing
enterRoom jid uuid =
    MkPresence
        { pFrom     = Nothing
        , pTo       = Just $ SomeJID jid
        , pId       = toString uuid
        , pType     = Default
        , pShowType = Available
        , pStatus   = ""
        , pPriority = Nothing
        , pExt   = [xml|<x xmlns="http://jabber.org/protocol/muc">|]
        , pPurpose = SOutgoing
        }

leaveRoom :: JID 'NodeResource -> UUID -> Stanza 'Presence 'Outgoing
leaveRoom jid uuid =
    MkPresence
        { pFrom     = Nothing
        , pTo       = Just $ SomeJID jid
        , pId       = toString uuid
        , pType     = Unavailable
        , pShowType = Available
        , pStatus   = ""
        , pPriority = Nothing
        , pExt   = []
        , pPurpose = SOutgoing
        }

destroyRoom :: JID 'NodeResource -> JID 'Resource -> UUID -> Stanza 'IQ 'Outgoing
destroyRoom from to uuid =
    MkIQ
        { iqFrom = Just $ SomeJID from
        , iqTo   = Just $ SomeJID to
        , iqId   = toString uuid
        , iqType = Set
        , iqBody = [xml| <query xmlns="http://jabber.org/protocol/muc#owner"> |]
        , iqPurpose = SOutgoing
        }

  -- <query xmlns='http://jabber.org/protocol/muc#owner'>
  --   <destroy jid='coven@chat.shakespeare.lit'>
  --     <reason>Macbeth doth come.</reason>
  --   </destroy>
  -- </query>

