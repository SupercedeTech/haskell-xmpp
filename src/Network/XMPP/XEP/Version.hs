{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

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
import Network.XMPP.Print    
import Network.XMPP.Utils 

import Text.XML.HaXml
    
-- | True, if stanza is a version request
isVersionReq :: Stanza 'IQ -> Bool               
isVersionReq (MkIQ { iqBody = ext }) =
    isVal "jabber:iq:version" "/iq/query/@xmlns" ext

-- | Replies to version request
versionAnswer :: String -> String -> String -> Stanza 'IQ -> [CFilter i]
versionAnswer name version os (MkIQ { }) =
    [ ptag "query"
               [ xmlns "jabber:iq:version" ]
               [ ptag "name" [] [literal name],
                 ptag "version" [] [literal version],
                 ptag "os" [] [literal os]
               ]
    ]
