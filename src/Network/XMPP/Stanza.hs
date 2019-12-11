{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Stanza
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  k.pierre.k@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- XMPP stanzas parsing 
--
-----------------------------------------------------------------------------

module Network.XMPP.Stanza
  (
    parse
  , parseM
  , outStanza
  ) where 

import Text.XML.HaXml.Xtract.Parse (xtract)

import Network.XMPP.Types
import Network.XMPP.Print
import Network.XMPP.Stream
import Network.XMPP.Utils

import Control.Applicative (Alternative, empty, pure)
import Data.Maybe

-- | Parses XML element producing Stanza
parse :: (Alternative l) => XmppMessage -> l SomeStanza
parse m
    | xtractp id "/message" m  = pure (SomeStanza (parseMessage m))
    | xtractp id "/presence" m = pure (SomeStanza (parsePresence m))
    | xtractp id "/iq" m       = pure (SomeStanza (parseIQ m))
    | otherwise                = empty
 where
  xtractp f p m = not . null $ xtract f p m

-- | Gets next message from stream and parses it
-- | We shall skip over unknown messages, rather than crashing
parseM :: XmppStateT SomeStanza
parseM = parse <$> nextM >>= \case
    Nothing -> parseM 
    Just v  -> pure v

parseMessage :: XmppMessage -> Stanza 'Message
parseMessage m = MkMessage from to id' messageType subject body thread x
  where
    from = mread $ txt "/message/@from" m
    to = read $ getText_ $ xtract id "/message/@to" m
    id' = getText_ $ xtract id "/message/@id" m
    messageType = read $ getText_ $ xtract id "/message/@type" m
    subject = getText_ $ xtract id "/message/subject/-" m
    body = getText_ $ xtract id "/message/body/-" m
    thread = getText_ $ xtract id "/message/thread/-" m
    x = xtract id "/message/*" m

parsePresence :: XmppMessage -> Stanza 'Presence
parsePresence m = MkPresence from to id' presenceType showType status priority x
  where
    from = mread $ txt "/presence/@from" m
    to = mread $ txt "/presence/@to" m
    id' = txt "/presence/@id" m
    presenceType = read $ txt "/presence/@type" m          
    showType = read $ txt "/presence/show/-" m
    status = txt "/presence/status/-" m
    priority = mread $ txt "/presence/priority/-" m
    x = xtract id "/presence/*" m

parseIQ :: XmppMessage -> Stanza 'IQ
parseIQ m = MkIQ from to id' iqType body
  where
    from = mread $ txt "/iq/@from" m
    to = mread $ txt "/iq/@to" m
    id' = txt "/iq/@id" m
    iqType = read $ txt "/iq/@type" m
    body = [m]

-- | Extract text from `XmppMessage' with supplied pattern
txt :: String      -- ^ xtract-like pattern to match
    -> XmppMessage -- ^ message being processed
    -> String      -- ^ result of extraction
txt p m = getText_ $ xtract id p m

-- | Converts stanza to XML and outputs it 
outStanza :: Stanza a -> XmppStateT ()
outStanza a@(MkMessage{})  = outMessage a
outStanza a@(MkPresence{}) = outPresence a
outStanza a@(MkIQ{})       = outIQ a

outMessage :: Stanza 'Message -> XmppStateT ()
outMessage (MkMessage from' to' id' mtype' subject' body' thread' x') =
    out $ toContent $ 
        ptag "message"
                ( (mattr "from" from') ++ 
                [ strAttr "to" (show to'),
                  strAttr "id" id',
                  strAttr "type" (show mtype'),
                  xmllang "en" ] )
                ([ ptag "body" [] [literal body'] ] ++
                 (if subject' /= "" then [ ptag "subject" [] [literal subject'] ] else []) ++                      
                 (if thread' /= "" then [ ptag "thread" [] [literal thread'] ] else []) ++
                 (map toFilter x'))

outPresence :: Stanza 'Presence -> XmppStateT ()
outPresence (MkPresence from' to' id' ptype' stype' status' priority' x') =
  out $ toContent $
      ptag "presence"
              ((mattr "from" from') ++
               (mattr "to" to') ++
               (if id' /= "" then [strAttr "id" id'] else []) ++
               (if ptype' /= Default then [strAttr "type" (show ptype')] else []) ++
               [xmllang "en" ])              
              ((if stype' /= Available then [ ptag "show" [] [literal $ show stype'] ] else []) ++
               (if status' /= "" then [ ptag "status" [] [ literal status' ] ]  else []) ++
               (if isJust priority' then [ ptag "priority" [] [ literal $ show (fromJust priority') ] ] else []) ++
               (map toFilter x'))

outIQ :: Stanza 'IQ -> XmppStateT ()
outIQ (MkIQ from' to' id' itype' body') =
  out $ toContent $
      ptag "iq"
               ((mattr "from" from') ++
                (mattr "to" to') ++       
                [ strAttr "id" id',
                  sattr "type" (show itype'),
                  xmllang "en" ])
               (map toFilter body')               

