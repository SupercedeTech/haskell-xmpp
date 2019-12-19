-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP
-- Copyright   :  (c) Dmitry Astapov, 2006 ; pierre, 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience module that re-exports all things XMPP
--
-----------------------------------------------------------------------------

module Network.XMPP
 ( module Network.XMPP.Sasl
 , module Network.XMPP.Core
 , module Network.XMPP.Helpers
 , module Network.XMPP.Print
 , module Network.XMPP.Types
 , module Network.XMPP.Stream
 , module Network.XMPP.Stanza
 , module Network.XMPP.IM.Presence
 , version
 ) where

import Network.XMPP.Sasl
import Network.XMPP.Core
import Network.XMPP.Helpers
import Network.XMPP.Print
import Network.XMPP.Types
import Network.XMPP.Stream
import Network.XMPP.Stanza

-- IM utilities modules
import Network.XMPP.IM.Presence

version :: String
version = "0.4-alpha"
