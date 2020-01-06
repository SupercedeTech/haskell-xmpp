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
( parse, parseM, waitAndProcess
, withUUID, outStanza
, StanzaEncoder(..), StanzaDecoder(..)
) where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Applicative         (Alternative, empty, pure)
import           Data.Maybe                  (mapMaybe)
import qualified Data.UUID.V4                as UUID
import qualified Data.UUID                   as UUID
import qualified Data.Text                   as T
import           Text.Hamlet.XML             (xml)
import           Text.XML                    (Node)
import           Text.XML.HaXml              (Content)
import           Text.XML.HaXml.Posn         (Posn)
import           Text.XML.HaXml.Xtract.Parse (xtract)
import           Network.XMPP.Stream
import           Network.XMPP.Types
import           Network.XMPP.Utils

-- | Parses XML element producing Stanza
parse :: forall l e. (Alternative l, FromXML e) => Content Posn -> l (SomeStanza e)
parse m | xtractp id "/message" m  = mSucceed (decodeStanza m :: Maybe (Stanza 'Message 'Incoming e))
        | xtractp id "/presence" m = mSucceed (decodeStanza m :: Maybe (Stanza 'Presence 'Incoming e))
        | xtractp id "/iq" m       = mSucceed (decodeStanza m :: Maybe (Stanza 'IQ 'Incoming e))
        | otherwise                = empty
  where xtractp f p m = not . null $ xtract f p m
        mSucceed :: (Alternative l, FromXML e) => Maybe (Stanza t p e) -> l (SomeStanza e)
        mSucceed = maybe empty (pure . SomeStanza)

-- | Gets next message from stream and parses it
-- | We shall skip over unknown messages, rather than crashing
parseM :: FromXML e => XmppMonad (SomeStanza e)
parseM = (parse <$> nextM) >>= maybe parseM pure

-- | Skips all messages, that will return result `Nothing` from computation
-- | In other words - waits for appropriate message, defined by predicate
waitAndProcess :: FromXML e => (SomeStanza e -> Maybe a) -> XmppMonad a
waitAndProcess compute = (compute <$> parseM) >>= maybe (waitAndProcess compute) pure

-- | Extract text from `Content Posn' with supplied pattern
txt :: T.Text      -- ^ xtract-like pattern to match
    -> Content Posn -- ^ message being processed
    -> T.Text      -- ^ result of extraction
txt p m = getText_ $ xtract id (T.unpack p) m

-- | Converts stanza to XML and outputs it
outStanza :: (StanzaEncoder t 'Outgoing e Node) => Stanza t 'Outgoing  e-> XmppMonad ()
outStanza = out . encodeStanza

withUUID :: MonadIO m => (UUID.UUID -> Stanza t p e) -> m (Stanza t p e)
withUUID setUUID = setUUID <$> liftIO UUID.nextRandom

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

instance StanzaEncoder 'Message 'Outgoing e Node where
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

instance StanzaEncoder 'Presence 'Outgoing e Node where
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

instance StanzaEncoder 'IQ 'Outgoing e Node where
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

instance FromXML e => StanzaDecoder 'Message 'Incoming e (Content Posn) where
  decodeStanza m = Just $ MkMessage
    { mFrom    = mread $ T.unpack $ txt "/message/@from" m
    , mTo      = read $ T.unpack $ getText_ $ xtract id "/message/@to" m
    , mId      = getText_ $ xtract id "/message/@id" m
    , mType    = read $ T.unpack $ getText_ $ xtract id "/message/@type" m
    , mSubject = getText_ $ xtract id "/message/subject/-" m
    , mBody    = getText_ $ xtract id "/message/body/-" m
    , mThread  = getText_ $ xtract id "/message/thread/-" m
    , mExt     = maybe (Left $ xtract id "/message/*" m) Right $ decodeXml m 
    , mPurpose = SIncoming
    }

instance FromXML e => StanzaDecoder 'Presence 'Incoming e (Content Posn) where
  decodeStanza m = Just $ MkPresence
    { pFrom     = mread $ T.unpack $ txt "/presence/@from" m
    , pTo       = mread $ T.unpack $ txt "/presence/@to" m
    , pId       = txt "/presence/@id" m
    , pType     = read $ T.unpack $ txt "/presence/@type" m
    , pShowType = read $ T.unpack $ txt "/presence/show/-" m
    , pStatus   = txt "/presence/status/-" m
    , pPriority = mread $ T.unpack $ txt "/presence/priority/-" m
    , pExt     = maybe (Left $ xtract id "/presence/*" m) Right $ decodeXml m 
    , pPurpose  = SIncoming
    }

instance FromXML e => StanzaDecoder 'IQ 'Incoming e (Content Posn) where
  decodeStanza m = Just MkIQ { iqFrom   = mread $ T.unpack $ txt "/iq/@from" m
                            , iqTo      = mread $ T.unpack $ txt "/iq/@to" m
                            , iqId      = txt "/iq/@id" m
                            , iqType    = read $ T.unpack $ txt "/iq/@type" m
                            , iqBody    = maybe (Left [m]) Right $ decodeXml m 
                            , iqPurpose = SIncoming
                            }

