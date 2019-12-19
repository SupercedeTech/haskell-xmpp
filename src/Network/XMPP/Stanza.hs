{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE QuasiQuotes           #-}
--These can disappear once we remove Content Posn versions
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}



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
, outStanza
, StanzaConverter(..)
) where 

import Text.Hamlet.XML (xml)
import Text.XML        (Node)
import qualified Data.Text as T

import Text.XML.HaXml              (Element(Elem), mkElemAttr, Content (CElem),
                                    QName(N))
import Text.XML.HaXml.Xtract.Parse (xtract)
import Text.XML.HaXml.Posn         (Posn, noPos)

import Network.XMPP.Types
import Network.XMPP.Stream
import Network.XMPP.Utils

import Control.Applicative (Alternative, empty, pure)

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
outStanza :: (StanzaConverter t (Content Posn)) => Stanza t -> XmppMonad ()
outStanza = out . convert

--------------------------------------------------------------------------------

class StanzaConverter t a where
    convert :: Stanza t -> a

noelem :: Content Posn
noelem = CElem (Elem (N "root") [] []) noPos

instance StanzaConverter 'Message (Content Posn) where
  convert MkMessage {..} =
    head $ ($ noelem)
        $ mkElemAttr
            "message"
            (  mattr "from" mFrom
            ++ [ strAttr "to"       (show mTo)
            , strAttr "id"       mId
            , strAttr "type"     (show mType)
            , strAttr "xml:lang" "en"
            ]
            )
        $ mkElemAttr "body" [] [literal mBody]
        :  [mkElemAttr "subject" [] [literal mSubject] | mSubject /= ""]
        ++ [mkElemAttr "thread" [] [literal mThread] | mThread /= ""]
        ++ map (const . (: [])) mExt

instance StanzaConverter 'Presence (Content Posn) where
    convert MkPresence{..} = head $ ($noelem) $
        mkElemAttr "presence"
            (  mattr "from" pFrom
            ++ mattr "to" pTo
            ++ [strAttr "id" pId | pId /= ""]
            ++ [strAttr "type" (show pType) | pType /= Default]
            ++ [strAttr "xml:lang" "en" ]
            ) $
               [mkElemAttr "show" [] [literal $ show pShowType] | pShowType /= Available]
            ++ [mkElemAttr "status" [] [literal pStatus] | pStatus /= ""]
            ++ case pPriority of
                    Just priority ->[ mkElemAttr "priority" [] [ literal $ show priority ]]
                    Nothing -> []
            ++ map (const . (: [])) pExt

instance StanzaConverter 'IQ (Content Posn) where
    convert MkIQ{..} = head $ ($noelem) $
        mkElemAttr "iq"
             (mattr "from" iqFrom
             ++ mattr "to" iqTo
             ++ [ strAttr "id" iqId
                , strAttr "type" $ show iqType
                , strAttr "xml:lang" "en" ])
             $ map (const . (: [])) iqBody

--------------------------------------------------------------------------------

instance StanzaConverter 'Message Node where
    convert MkMessage{..} =
        let _from    = T.pack $ maybe "" show mFrom
            _to      = T.pack $ show mTo
            _id      = T.pack   mId
            _type    = T.pack $ show mType
            _subject = T.pack   mSubject
            _body    = T.pack   mBody
            _thread  = T.pack   mThread
            fromAttr = maybe [] (\x -> [("from", show x)]) mFrom
            testAttr = [ ("subject", "_subject")
                       , ("thread", "_thread")
                       ]
        in head [xml|
            <message
                *{ fromAttr }
                to=#{ _to }
                id=#{ _id }
                type=#{ _type }
                xml:lang=en
                >
                <body
                    *{ testAttr }
                    >#{ _body }
    |]
    --         ^{ mExt' }
    -- |]
    --

instance StanzaConverter 'Presence [Node] where
    convert MkPresence{..} = [xml|<x>|]
instance StanzaConverter 'IQ [Node] where
    convert MkIQ{..} = [xml|<x>|]

