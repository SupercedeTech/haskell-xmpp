{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.IQ
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  k.pierre.k@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- XMPP IQ utilites
--
-----------------------------------------------------------------------------

module Network.XMPP.IQ
  ( iqSend
  , iqReplyTo
  ) where 

import Network.XMPP.Types
import Network.XMPP.Stanza
import Network.XMPP.Concurrent
import Text.XML (Node)

import Text.XML.HaXml
import Text.XML.HaXml.Posn

noelem :: Content Posn
noelem = CElem (Elem (N "root") [] []) noPos

-- | Send IQ of specified type with supplied data
iqSend :: String -- ^ ID to use
       -> IQType -- ^ IQ type
       -> [Node] -- ^ request contents 
       -> XmppMonad ()
iqSend id t body = outStanza $ MkIQ Nothing Nothing id t body SOutgoing

-- Extract IQ reply that matches the supplied predicate from the event stream and send it (transformed)        
iqReplyTo :: (Stanza 'IQ 'Incoming -> Bool) -- ^ Predicate used to match required IQ reply
          -> (Stanza 'IQ 'Incoming -> [CFilter Posn]) -- ^ transformer function
          -> XmppThreadT ()
iqReplyTo p t = do
  s <- waitFor (\case
            SomeStanza xiq@MkIQ{ iqPurpose = SIncoming } -> p xiq
            _                     -> False)
  case s of
    SomeStanza stnz@MkIQ{ iqPurpose = SIncoming } -> writeChanS $ SomeStanza $ transform t stnz
    _                                             -> pure ()
    where
      transform :: (Stanza 'IQ 'Incoming -> [CFilter Posn]) -> Stanza 'IQ 'Incoming -> Stanza 'IQ 'Incoming
      transform t s@(MkIQ from' to' id' _type' _body' SIncoming) =
          MkIQ to' from' id' Result (map (head . ($noelem)) $ t s) SIncoming
