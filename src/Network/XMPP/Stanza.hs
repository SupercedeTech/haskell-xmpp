{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
--These can disappear once we remove Content Posn versions
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



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

import           Control.Applicative         (Alternative, empty, pure)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (pack)
import           Text.Hamlet.XML             (xml)
import           Text.XML                    (Node)
import           Text.XML.HaXml              (Content)
import           Text.XML.HaXml.Posn         (Posn)
import           Text.XML.HaXml.Xtract.Parse (xtract)
import           Network.XMPP.Stream
import           Network.XMPP.Types
import           Network.XMPP.Utils

-- | Parses XML element producing Stanza
parse :: Alternative l => Content Posn -> l SomeStanza
parse m | xtractp id "/message" m  = pure $ SomeStanza $ parseMessage m
        | xtractp id "/presence" m = pure $ SomeStanza $ parsePresence m
        | xtractp id "/iq" m       = pure $ SomeStanza $ parseIQ m
        | otherwise                = empty
  where xtractp f p m = not . null $ xtract f p m

-- | Gets next message from stream and parses it
-- | We shall skip over unknown messages, rather than crashing
parseM :: XmppMonad SomeStanza
parseM = parse <$> nextM >>= \case
    Nothing -> parseM
    Just v  -> pure v

parseMessage :: Content Posn -> Stanza 'Message 'Incoming
parseMessage m = MkMessage
    { mFrom    = mread $ txt "/message/@from" m
    , mTo      = read $ getText_ $ xtract id "/message/@to" m
    , mId      = getText_ $ xtract id "/message/@id" m
    , mType    = read $ getText_ $ xtract id "/message/@type" m
    , mSubject = getText_ $ xtract id "/message/subject/-" m
    , mBody    = getText_ $ xtract id "/message/body/-" m
    , mThread  = getText_ $ xtract id "/message/thread/-" m
    , mExt     = xtract id "/message/*" m
    , mPurpose = SIncoming
    }

parsePresence :: Content Posn -> Stanza 'Presence 'Incoming
parsePresence m = MkPresence
    { pFrom     = mread $ txt "/presence/@from" m
    , pTo       = mread $ txt "/presence/@to" m
    , pId       = txt "/presence/@id" m
    , pType     = read $ txt "/presence/@type" m
    , pShowType = read $ txt "/presence/show/-" m
    , pStatus   = txt "/presence/status/-" m
    , pPriority = mread $ txt "/presence/priority/-" m
    , pExt      = xtract id "/presence/*" m
    , pPurpose = SIncoming
    }

parseIQ :: Content Posn -> Stanza 'IQ 'Incoming
parseIQ m = MkIQ
    { iqFrom = mread $ txt "/iq/@from" m
    , iqTo   = mread $ txt "/iq/@to" m
    , iqId   = txt "/iq/@id" m
    , iqType = read $ txt "/iq/@type" m
    , iqBody = [m]
    , iqPurpose = SIncoming
    }

-- | Extract text from `Content Posn' with supplied pattern
txt :: String      -- ^ xtract-like pattern to match
    -> Content Posn -- ^ message being processed
    -> String      -- ^ result of extraction
txt p m = getText_ $ xtract id p m

-- | Converts stanza to XML and outputs it
outStanza :: (StanzaConverter t Node) => Stanza t 'Outgoing -> XmppMonad ()
outStanza = out . convert

--------------------------------------------------------------------------------

class StanzaConverter t a where
    convert :: Stanza t p -> a

--------------------------------------------------------------------------------

condToAlt :: Alternative m => (x -> Bool) -> x -> m x
condToAlt f x = if f x then pure x else empty

toAttrList :: Traversable t => [t (Maybe a)] -> [t a]
toAttrList = mapMaybe sequence

instance StanzaConverter 'Message Node where
  convert MkMessage{..} = head [xml|
    <message *{messageAttrs} xml:lang=en>
      <body *{bodyAttrs}>
        #{pack mBody}
  |]
    where
      messageAttrs = toAttrList
        [ ("from", show <$> mFrom)
        , ("to", Just $ show mTo)
        , ("id", Just mId)
        , ("type", Just $ show mType)
        ]
      bodyAttrs = toAttrList
        [ ("subject", condToAlt (not . null) mSubject)
        , ("thread", condToAlt (not . null) mThread)
        ]

instance StanzaConverter 'Presence Node where
  convert MkPresence{ pPurpose = SOutgoing, ..} = head [xml|
    <presence *{attrs} xml:lang=en>
      ^{pExt}
  |]
    where
      attrs = toAttrList
        [ ("from", show <$> pFrom)
        , ("to", show <$> pTo)
        , ("id", condToAlt (not . null) pId)
        , ("type", show <$> condToAlt (/=Default) pType)
        , ("show", show <$> condToAlt (/=Available) pShowType)
        , ("status", condToAlt (not . null) pStatus)
        , ("priority", show <$> pPriority)
        ]
  convert _ = head [xml|<error>Unsupported message for conversion|] -- TODO: define appropriate class instance

instance StanzaConverter 'IQ Node where
  convert MkIQ{ iqPurpose = SOutgoing, ..} = head [xml|
    <iq *{attrs} xml:lang=en>
      ^{iqBody}
  |]
    where
      attrs = toAttrList
        [ ("from", show <$> iqFrom)
        , ("to", show <$> iqTo)
        , ("id", Just iqId)
        , ("type", Just $ show iqType)
        ]
  convert _ = head [xml|<error>Unsupported message for conversion|] -- TODO: define appropriate class instance