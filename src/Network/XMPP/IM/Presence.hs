{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.IM.Presence
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Copyright   :  (c) riskbook, 2020
-- SPDX-License-Identifier:  BSD3
-- 
-- Maintainer  :  k.pierre.k@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- XMPP presence utilities
--
-----------------------------------------------------------------------------
module Network.XMPP.IM.Presence
  (
    presAvailable
  , presUnavailable
  , presAway
  , presXa
  , presChat
  , presDND
  ) where

import Network.XMPP.Types
import Data.Text           (Text)

-- | Default presence, should be sent at first
presAvailable :: Text -- ^ Status message
              -> Stanza 'Presence 'Outgoing ()
presAvailable status = MkPresence Nothing Nothing "" Default Available status (Just 0) [] SOutgoing

-- | Should be sent at last
presUnavailable :: Text -> Stanza 'Presence 'Outgoing ()
presUnavailable = mkPresenceU Available

presAway :: Text -> Stanza 'Presence 'Outgoing ()
presAway = mkPresenceD Away

presXa :: Text -> Stanza 'Presence 'Outgoing ()
presXa = mkPresenceD XAway

presChat :: Text -> Stanza 'Presence 'Outgoing ()
presChat = mkPresenceD FreeChat

presDND :: Text -> Stanza 'Presence 'Outgoing ()
presDND = mkPresenceD DND

-- | Helper to construct presence Stanza with required attrs
mkPresence :: PresenceType -> ShowType -> Text -> Stanza 'Presence 'Outgoing ()
mkPresence typ showType status = 
    MkPresence 
        { pFrom     = Nothing
        , pTo       = Nothing
        , pId       = ""
        , pType     = typ
        , pShowType = showType
        , pStatus   = status
        , pPriority = Just 0
        , pExt      = []
        , pPurpose = SOutgoing
        }

mkPresenceD :: ShowType -> Text -> Stanza 'Presence 'Outgoing ()
mkPresenceD = mkPresence Default

mkPresenceU :: ShowType -> Text -> Stanza 'Presence 'Outgoing ()
mkPresenceU = mkPresence Unavailable
