{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.XEP.Version
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- XEP-0092, version request
--
-----------------------------------------------------------------------------
module Network.XMPP.XEP.Version
    ( isVersionReq
    , versionAnswer
    ) where

import Network.XMPP.Types
import Network.XMPP.Utils

import Text.XML.HaXml
    
-- | True, if stanza is a version request
isVersionReq :: Stanza 'IQ 'Incoming () -> Bool
isVersionReq MkIQ { iqBody = ext } =
    either (isVal "jabber:iq:version" "/iq/query/@xmlns") (const False) ext

-- | Replies to version request
versionAnswer :: String -> String -> String -> Stanza 'IQ 'Outgoing () -> [CFilter i]
versionAnswer name version os MkIQ { } =
    [ mkElemAttr "query"
       [ strAttr "xmlns" "jabber:iq:version" ]
       [ mkElemAttr "name" [] [literal name],
         mkElemAttr "version" [] [literal version],
         mkElemAttr "os" [] [literal os]
       ]
    ]
