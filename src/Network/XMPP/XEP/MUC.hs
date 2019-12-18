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

import Text.XML.HaXml              (Element(Elem), mkElemAttr, Content (CElem),
                                    QName(N))
import Text.XML.HaXml.Posn         (Posn, noPos)
import Network.XMPP.Types
import Network.XMPP.Utils


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
noelem :: Content Posn
noelem = CElem (Elem (N "root") [] []) noPos

enterRoom :: JID '[] -> UUID -> Stanza 'Presence
enterRoom jid uuid =
    MkPresence
        { pFrom     = Nothing
        , pTo       = (Just jid)
        , pId       = toString uuid
        , pType     = Default
        , pShowType = Available
        , pStatus   = ""
        , pPriority = Nothing
--        , pExt      = [xml|
--                        <x xmlns='http://jabber.org/protocol/muc'/>
--                      |]
        , pExt   = [ head $ ($noelem) $
                       mkElemAttr "x"
                        [ strAttr "xmlns" "http://jabber.org/protocol/muc" ]
                        []
                   ]
        }

leaveRoom :: JID '[] -> UUID -> Stanza 'Presence
leaveRoom jid uuid =
    MkPresence
        { pFrom     = Nothing
        , pTo       = (Just jid)
        , pId       = toString uuid
        , pType     = Unavailable
        , pShowType = Available
        , pStatus   = ""
        , pPriority = Nothing
        , pExt      = []
        }

destroyRoom :: JID '[] -> JID '[] -> UUID -> Stanza 'IQ
destroyRoom from to uuid =
    MkIQ
        { iqFrom = Just from
        , iqTo   = Just to
        , iqId   = toString uuid
        , iqType = Set
        , iqBody = [ head $ ($noelem) $
                       mkElemAttr "query"
                        [ strAttr "xmlns" "http://jabber.org/protocol/muc#owner" ]
                        []
                   ]
        }

  -- <query xmlns='http://jabber.org/protocol/muc#owner'>
  --   <destroy jid='coven@chat.shakespeare.lit'>
  --     <reason>Macbeth doth come.</reason>
  --   </destroy>
  -- </query>

