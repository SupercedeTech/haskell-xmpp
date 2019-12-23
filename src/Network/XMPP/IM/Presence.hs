{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.IM.Presence
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
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

-- | Default presence, should be sent at first
presAvailable :: String -- ^ Status message
              -> Stanza 'Presence 'Outgoing
presAvailable status = MkPresence Nothing Nothing "" Default Available status (Just 0) [] SOutgoing

-- | Should be sent at last
presUnavailable :: String -> Stanza 'Presence 'Outgoing
presUnavailable = mkPresenceU Available

presAway :: String -> Stanza 'Presence 'Outgoing
presAway = mkPresenceD Away

presXa :: String -> Stanza 'Presence 'Outgoing
presXa = mkPresenceD XAway

presChat :: String -> Stanza 'Presence 'Outgoing
presChat = mkPresenceD FreeChat

presDND :: String -> Stanza 'Presence 'Outgoing
presDND = mkPresenceD DND

-- | Helper to contruct presence Stanza with required attrs
mkPresence :: PresenceType -> ShowType -> String -> Stanza 'Presence 'Outgoing
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

mkPresenceD :: ShowType -> String -> Stanza 'Presence 'Outgoing
mkPresenceD = mkPresence Default

mkPresenceU :: ShowType -> String -> Stanza 'Presence 'Outgoing
mkPresenceU = mkPresence Unavailable
