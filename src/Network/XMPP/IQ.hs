{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

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
import Network.XMPP.Utils
import Network.XMPP.Concurrent
    
import Text.XML.HaXml
import Text.XML.HaXml.Posn    

-- | Send IQ of specified type with supplied data
iqSend :: String -- ^ ID to use
       -> IQType -- ^ IQ type
       -> [CFilter Posn] -- ^ request contents 
       -> XmppMonad ()
iqSend id t d = do
  outStanza $ MkIQ Nothing Nothing id t (map toContent d)               

-- Extract IQ reply that matches the supplied predicate from the event stream and send it (transformed)        
iqReplyTo :: (Stanza 'IQ -> Bool) -- ^ Predicate used to match required IQ reply
          -> (Stanza 'IQ -> [CFilter Posn]) -- ^ transformer function
          -> XmppThreadT ()
iqReplyTo p t = do
  s <- waitFor (\x -> case x of
                        SomeStanza xiq@(MkIQ{}) -> p xiq
                        _                       -> False)
  case s of
    SomeStanza stnz@(MkIQ{}) -> writeChanS (SomeStanza (transform t stnz))
    _                        -> pure ()
    where
      transform :: (Stanza 'IQ -> [CFilter Posn]) -> Stanza 'IQ -> Stanza 'IQ
      transform t s@(MkIQ from' to' id' type' body') =
          MkIQ to' from' id' Result (map toContent $ t s)
