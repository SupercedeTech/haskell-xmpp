{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds  #-}
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
import qualified Data.Text as T

import Text.XML.HaXml
import Text.XML.HaXml.Posn

noelem :: Content Posn
noelem = CElem (Elem (N "root") [] []) noPos

-- | Send IQ of specified type with supplied data
iqSend :: T.Text -- ^ ID to use
       -> IQType -- ^ IQ type
       -> [Node] -- ^ request contents 
       -> XmppMonad ()
iqSend id t body = outStanza $ MkIQ Nothing Nothing id t body SOutgoing

-- Extract IQ reply that matches the supplied predicate from the event stream and send it (transformed)        
iqReplyTo :: (Stanza 'IQ 'Incoming e -> Bool) -- ^ Predicate used to match required IQ reply
          -> (Stanza 'IQ 'Incoming e -> [CFilter Posn]) -- ^ transformer function
          -> XmppThreadT () e
iqReplyTo p t = do
  s <- waitFor (\case
            SomeStanza xiq@MkIQ{ iqPurpose = SIncoming } -> p xiq
            _                     -> False)
  case s of
    SomeStanza stnz@MkIQ{ iqPurpose = SIncoming } -> writeChanS $ SomeStanza $ transform t stnz
    _                                             -> pure ()
    where
      transform :: (Stanza 'IQ 'Incoming e -> [CFilter Posn]) -> Stanza 'IQ 'Incoming e -> Stanza 'IQ 'Incoming e
      transform t s@(MkIQ from' to' id' _type' _body' SIncoming) =
          MkIQ to' from' id' Result (Left $ map (head . ($noelem)) $ t s) SIncoming
