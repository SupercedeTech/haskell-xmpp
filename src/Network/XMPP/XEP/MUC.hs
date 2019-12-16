{-# LANGUAGE DataKinds #-}

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
( mucJoinStanza, mucLeaveStanza
) where

import Data.UUID           (UUID, toString)

import Network.XMPP.Stanza
import Network.XMPP.Types
import Network.XMPP.Print    
import Network.XMPP.JID
import Network.XMPP.Utils

mucJoinStanza :: JID -> UUID -> Stanza 'Presence
mucJoinStanza jid uuid =
    MkPresence
        { pFrom     = Nothing
        , pTo       = (Just jid)
        , pId       = toString uuid
        , pType     = Default
        , pShowType = Available
        , pStatus   = ""
        , pPriority = Nothing
        , pExt      = [ toContent $
                            itag "x" [ xmlns "http://jabber.org/protocol/muc" ]
                      ]
        }

mucLeaveStanza :: JID -> UUID -> Stanza 'Presence
mucLeaveStanza jid uuid =
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
