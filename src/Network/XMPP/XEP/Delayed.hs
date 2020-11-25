{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.XEP.Delayed
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Copyright   :  (c) riskbook, 2020
-- SPDX-License-Identifier:  BSD3
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- XEP-0091, old delayed delivery
--
-----------------------------------------------------------------------------
module Network.XMPP.XEP.Delayed
  (
    isDelayed
  ) where

import Network.XMPP.Stream
import Network.XMPP.Types

import Text.XML.HaXml.Xtract.Parse (xtract)

-- | True, if stanza is delivered delayed
isDelayed :: Stanza a 'Incoming () -> Bool
isDelayed (MkMessage _ _ _ _ _ _ _ ext _) =
  case ext of
    Right () -> False
    Left c -> any (== "jabber:x:delay") $ map (getText_ . xtract id "/x/@xmlns") c
isDelayed _ = False
