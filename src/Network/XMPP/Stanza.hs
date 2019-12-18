{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

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
( parse, parseM, parseMessage, parsePresence, parseIQ
, outStanza, outMessage, outPresence, outIQ
) where 

import Text.XML.HaXml              (mkElemAttr)
import Text.XML.HaXml.Xtract.Parse (xtract)
import Text.XML.HaXml.Types        (Content)
import Text.XML.HaXml.Posn         (Posn)

import Network.XMPP.Types
import Network.XMPP.Print
import Network.XMPP.Stream
import Network.XMPP.Utils

import Control.Applicative (Alternative, empty, pure)
import Data.Maybe

-- | Parses XML element producing Stanza
parse :: (Alternative l) => Content Posn -> l SomeStanza
parse m
    | xtractp id "/message" m  = pure (SomeStanza (parseMessage m))
    | xtractp id "/presence" m = pure (SomeStanza (parsePresence m))
    | xtractp id "/iq" m       = pure (SomeStanza (parseIQ m))
    | otherwise                = empty
 where
  xtractp f p m = not . null $ xtract f p m

-- | Gets next message from stream and parses it
-- | We shall skip over unknown messages, rather than crashing
parseM :: XmppMonad SomeStanza
parseM = parse <$> nextM >>= \case
    Nothing -> parseM 
    Just v  -> pure v

parseMessage :: Content Posn -> Stanza 'Message
parseMessage m = MkMessage
    { mFrom    = mread $ txt "/message/@from" m
    , mTo      = read $ getText_ $ xtract id "/message/@to" m
    , mId      = getText_ $ xtract id "/message/@id" m
    , mType    = read $ getText_ $ xtract id "/message/@type" m
    , mSubject = getText_ $ xtract id "/message/subject/-" m
    , mBody    = getText_ $ xtract id "/message/body/-" m
    , mThread  = getText_ $ xtract id "/message/thread/-" m
    , mExt     = xtract id "/message/*" m
    }

parsePresence :: Content Posn -> Stanza 'Presence
parsePresence m = MkPresence
    { pFrom     = mread $ txt "/presence/@from" m
    , pTo       = mread $ txt "/presence/@to" m
    , pId       = txt "/presence/@id" m
    , pType     = read $ txt "/presence/@type" m          
    , pShowType = read $ txt "/presence/show/-" m
    , pStatus   = txt "/presence/status/-" m
    , pPriority = mread $ txt "/presence/priority/-" m
    , pExt      = xtract id "/presence/*" m
    }

parseIQ :: Content Posn -> Stanza 'IQ
parseIQ m = MkIQ
    { iqFrom = mread $ txt "/iq/@from" m
    , iqTo   = mread $ txt "/iq/@to" m
    , iqId   = txt "/iq/@id" m
    , iqType = read $ txt "/iq/@type" m
    , iqBody = [m]
    }

-- | Extract text from `Content Posn' with supplied pattern
txt :: String      -- ^ xtract-like pattern to match
    -> Content Posn -- ^ message being processed
    -> String      -- ^ result of extraction
txt p m = getText_ $ xtract id p m

-- | Converts stanza to XML and outputs it 
outStanza :: Stanza a -> XmppMonad ()
outStanza a@(MkMessage{})  = outMessage a
outStanza a@(MkPresence{}) = outPresence a
outStanza a@(MkIQ{})       = outIQ a

outMessage :: Stanza 'Message -> XmppMonad ()
outMessage (MkMessage from' to' id' mtype' subject' body' thread' x') =
    out $ toContent $ 
        mkElemAttr "message"
                ( (mattr "from" from') ++ 
                [ strAttr "to" (show to'),
                  strAttr "id" id',
                  strAttr "type" (show mtype'),
                  xmllang "en" ] )
                ([ mkElemAttr "body" [] [literal body'] ] ++
                 (if subject' /= "" then [ mkElemAttr "subject" [] [literal subject'] ] else []) ++                      
                 (if thread' /= "" then [ mkElemAttr "thread" [] [literal thread'] ] else []) ++
                 (map toFilter x'))

outPresence :: Stanza 'Presence -> XmppMonad ()
outPresence (MkPresence from' to' id' ptype' stype' status' priority' x') =
  out $ toContent $
      mkElemAttr "presence"
              ((mattr "from" from') ++
               (mattr "to" to') ++
               (if id' /= "" then [strAttr "id" id'] else []) ++
               (if ptype' /= Default then [strAttr "type" (show ptype')] else []) ++
               [xmllang "en" ])              
              ((if stype' /= Available then [ mkElemAttr "show" [] [literal $ show stype'] ] else []) ++
               (if status' /= "" then [ mkElemAttr "status" [] [ literal status' ] ]  else []) ++
               (if isJust priority' then [ mkElemAttr "priority" [] [ literal $ show (fromJust priority') ] ] else []) ++
               (map toFilter x'))

outIQ :: Stanza 'IQ -> XmppMonad ()
outIQ MkIQ{..} =
  out $ toContent $
      mkElemAttr "iq"
               ((mattr "from" iqFrom) ++
                (mattr "to" iqTo) ++       
                [ strAttr "id" iqId,
                  sattr "type" (show iqType),
                  xmllang "en" ])
               (map toFilter iqBody)               

