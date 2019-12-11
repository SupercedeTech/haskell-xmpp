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
              -> Stanza 'Presence
presAvailable status = MkPresence Nothing Nothing "" Default Available status (Just 0) []

-- | Should be sent at last
presUnavailable :: String -> Stanza 'Presence
presUnavailable status = mkPresenceU Available status

presAway :: String -> Stanza 'Presence
presAway status = mkPresenceD Away status
                  
presXa :: String -> Stanza 'Presence
presXa status = mkPresenceD XAway status

presChat :: String -> Stanza 'Presence
presChat status = mkPresenceD FreeChat status

presDND :: String -> Stanza 'Presence
presDND status = mkPresenceD DND status

-- | Helper to contruct presence Stanza with required attrs
mkPresence :: PresenceType -> ShowType -> String -> Stanza 'Presence
mkPresence typ showType status = 
  MkPresence Nothing -- pFrom
             Nothing -- PTo
             ""      -- pId
             typ
             showType
             status
             (Just 0) -- pPriority :: Maybe Integer
             []       -- pExt :: [Content Posn]

mkPresenceD :: ShowType -> String -> Stanza 'Presence
mkPresenceD = mkPresence Default

mkPresenceU :: ShowType -> String -> Stanza 'Presence
mkPresenceU = mkPresence Unavailable
