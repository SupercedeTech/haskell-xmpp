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
  (
    mucJoin
  , mucLeave
  , mucJoinStanza
  , mucLeaveStanza
  ) where

import Network.XMPP.Stanza
import Network.XMPP.Types
import Network.XMPP.Print    
import Network.XMPP.JID
import Network.XMPP.Utils

-- | Joins MUC room named by JID (conference\@server\/nick)
mucJoin :: JID -> XmppStateT ()
mucJoin jid = do
  outStanza $ mucJoinStanza jid

-- | Leaves MUC room named by JID (conference\@server\/nick)
mucLeave :: JID -> XmppStateT ()
mucLeave jid = do
  outStanza $ mucLeaveStanza jid

-- | Stanza sent by 'mucJoin'
mucJoinStanza :: JID -> Stanza 'Presence
mucJoinStanza jid =
    MkPresence Nothing (Just jid) "123" Default Available "" Nothing 
                 [ toContent $
                   itag "x" [ xmlns "http://jabber.org/protocol/muc" ]
                 ]

-- | Stanza sent by 'mucLeave'
mucLeaveStanza :: JID -> Stanza 'Presence
mucLeaveStanza jid =
    MkPresence Nothing (Just jid) "" Unavailable Available "" Nothing []
