{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
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
  ( StanzaEncoder(..)
  , StanzaDecoder(..)
  ) where

import           Control.Applicative         (Alternative, empty, pure)
import           Data.Maybe                  (mapMaybe, listToMaybe)
import qualified Data.Text                   as T
import           Text.Hamlet.XML             (xml)
import           Text.XML                    (Node)
import           Text.XML.HaXml              (Content)
import           Text.XML.HaXml.Posn         (Posn)
import           Text.XML.HaXml.Xtract.Parse (xtract)
import           Network.XMPP.Types
import           Network.XMPP.XML

--------------------------------------------------------------------------------

class StanzaEncoder t p e a where
  encodeStanza :: Stanza t p e -> a

class StanzaDecoder t p e a where
  decodeStanza :: a -> Maybe (Stanza t p e)

--------------------------------------------------------------------------------

condToAlt :: Alternative m => (x -> Bool) -> x -> m x
condToAlt f x = if f x then pure x else empty

toAttrList :: [(String, Maybe a)] -> [(String, a)]
toAttrList = mapMaybe sequence

instance {-# OVERLAPPING #-} StanzaEncoder 'Message 'Outgoing e Node where
  encodeStanza MkMessage{..} = head [xml|
    <message *{messageAttrs} xml:lang=en>
      <body *{bodyAttrs}>
        #{mBody}
  |]
    where
      messageAttrs = toAttrList
        [ ("from", show <$> mFrom)
        , ("to", Just $ show mTo)
        , ("id", Just $ T.unpack mId)
        , ("type", Just $ show mType)
        ]
      bodyAttrs = toAttrList
        [ ("subject", T.unpack <$> condToAlt (not . T.null) mSubject)
        , ("thread", T.unpack <$> condToAlt (not . T.null) mThread)
        ]

instance {-# OVERLAPPING #-} StanzaEncoder 'Presence 'Outgoing e Node where
  encodeStanza MkPresence{ pPurpose = SOutgoing, ..} = head [xml|
    <presence *{attrs} xml:lang="en">
      ^{pExt}
    |]
    where
      attrs = toAttrList
        [ ("from", show <$> pFrom)
        , ("to", show <$> pTo)
        , ("id", T.unpack <$> condToAlt (not . T.null) pId)
        , ("type", show <$> condToAlt (/= Default) pType)
        , ("show", show <$> condToAlt (/= Available) pShowType)
        , ("status", T.unpack <$> condToAlt (not . T.null) pStatus)
        , ("priority", show <$> pPriority)
        ]

instance {-# OVERLAPPING #-} StanzaEncoder 'IQ 'Outgoing e Node where
  encodeStanza MkIQ{ iqPurpose = SOutgoing, ..} = head [xml|
    <iq *{attrs} xml:lang="en">
      ^{iqBody}
  |]
    where
      attrs = toAttrList
        [ ("from", show <$> iqFrom)
        , ("to", show <$> iqTo)
        , ("id", Just $ T.unpack iqId)
        , ("type", Just $ show iqType)
        ]

instance StanzaEncoder t 'Outgoing e Node where
  encodeStanza s@MkPresence{} = encodeStanza s
  encodeStanza s@MkMessage{}  = encodeStanza s
  encodeStanza s@MkIQ{}       = encodeStanza s

instance FromXML e => StanzaDecoder 'Message 'Incoming e (Content Posn) where
  decodeStanza m =
    let content = xtract id "/message/*" m
    in
      Just $ MkMessage
        { mFrom    = mread $ txtpat "/message/@from" m
        , mTo      = read $ T.unpack $ getText_ $ xtract id "/message/@to" m
        , mId      = getText_ $ xtract id "/message/@id" m
        , mType    = read $ T.unpack $ getText_ $ xtract id "/message/@type" m
        , mSubject = getText_ $ xtract id "/message/subject/-" m
        , mBody    = getText_ $ xtract id "/message/body/-" m
        , mThread  = getText_ $ xtract id "/message/thread/-" m
        , mExt     = maybe (Left content) Right $ listToMaybe $ mapMaybe decodeXml
                                                                        content
        , mPurpose = SIncoming
        }

instance FromXML e => StanzaDecoder 'Presence 'Incoming e (Content Posn) where
  decodeStanza m =
    let content = xtract id "/presence/*" m
    in
      Just $ MkPresence
        { pFrom     = mread $ txtpat "/presence/@from" m
        , pTo       = mread $ txtpat "/presence/@to" m
        , pId       = txtpat "/presence/@id" m
        , pType     = read $ T.unpack $ txtpat "/presence/@type" m
        , pShowType = read $ T.unpack $ txtpat "/presence/show/-" m
        , pStatus   = txtpat "/presence/status/-" m
        , pPriority = mread $ txtpat "/presence/priority/-" m
        , pPurpose  = SIncoming
        , pExt = maybe (Left content) Right $ listToMaybe $ mapMaybe decodeXml content
        }

instance FromXML e => StanzaDecoder 'IQ 'Incoming e (Content Posn) where
  decodeStanza m = Just MkIQ { iqFrom   = mread $ txtpat "/iq/@from" m
                            , iqTo      = mread $ txtpat "/iq/@to" m
                            , iqId      = txtpat "/iq/@id" m
                            , iqType    = read $ T.unpack $ txtpat "/iq/@type" m
                            , iqBody    = maybe (Left [m]) Right $ decodeXml m 
                            , iqPurpose = SIncoming
                            }

