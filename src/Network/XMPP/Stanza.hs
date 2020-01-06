{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE RecordWildCards       #-}
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
( parse, parseM
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
parse :: Alternative l => Content Posn -> l SomeStanza
parse m | xtractp id "/message" m  = mSucceed (decodeStanza m :: Maybe (Stanza 'Message 'Incoming))
        | xtractp id "/presence" m = mSucceed (decodeStanza m :: Maybe (Stanza 'Presence 'Incoming))
        | xtractp id "/iq" m       = mSucceed (decodeStanza m :: Maybe (Stanza 'IQ 'Incoming))
        | otherwise                = empty
  where xtractp f p m = not . null $ xtract f p m
        mSucceed :: Alternative l => Maybe (Stanza t p) -> l SomeStanza
        mSucceed = maybe empty (pure . SomeStanza)

-- | Gets next message from stream and parses it
-- | We shall skip over unknown messages, rather than crashing
parseM :: XmppMonad SomeStanza
parseM = (parse <$> nextM) >>= maybe parseM pure

waitForM :: FromXML a => XmppMonad (Either T.Text (SomeStanza, a))
waitForM = do
  stanza <- parseM
  let d = decodeXml stanza
  pure undefined

-- | Extract text from `Content Posn' with supplied pattern
txt :: T.Text      -- ^ xtract-like pattern to match
    -> Content Posn -- ^ message being processed
    -> T.Text      -- ^ result of extraction
txt p m = getText_ $ xtract id (T.unpack p) m

-- | Converts stanza to XML and outputs it
outStanza :: (StanzaEncoder t 'Outgoing Node) => Stanza t 'Outgoing -> XmppMonad ()
outStanza = out . encodeStanza

withUUID :: MonadIO m => (UUID.UUID -> Stanza t p) -> m (Stanza t p)
withUUID setUUID = setUUID <$> liftIO UUID.nextRandom

--------------------------------------------------------------------------------

class StanzaEncoder t p a where
  encodeStanza :: Stanza t p -> a

class StanzaDecoder t p a where
  decodeStanza :: a -> Maybe (Stanza t p)

--------------------------------------------------------------------------------

condToAlt :: Alternative m => (x -> Bool) -> x -> m x
condToAlt f x = if f x then pure x else empty

toAttrList :: [(String, Maybe a)] -> [(String, a)]
toAttrList = mapMaybe sequence

instance StanzaEncoder 'Message 'Outgoing Node where
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

instance StanzaEncoder 'Presence 'Outgoing Node where
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

instance StanzaEncoder 'IQ 'Outgoing Node where
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

instance StanzaDecoder 'Message 'Incoming (Content Posn) where
  decodeStanza m = Just $ MkMessage
    { mFrom    = mread $ T.unpack $ txt "/message/@from" m
    , mTo      = read $ T.unpack $ getText_ $ xtract id "/message/@to" m
    , mId      = getText_ $ xtract id "/message/@id" m
    , mType    = read $ T.unpack $ getText_ $ xtract id "/message/@type" m
    , mSubject = getText_ $ xtract id "/message/subject/-" m
    , mBody    = getText_ $ xtract id "/message/body/-" m
    , mThread  = getText_ $ xtract id "/message/thread/-" m
    , mExt     = xtract id "/message/*" m
    , mPurpose = SIncoming
    }

instance StanzaDecoder 'Presence 'Incoming (Content Posn) where
  decodeStanza m = Just $ MkPresence
    { pFrom     = mread $ T.unpack $ txt "/presence/@from" m
    , pTo       = mread $ T.unpack $ txt "/presence/@to" m
    , pId       = txt "/presence/@id" m
    , pType     = read $ T.unpack $ txt "/presence/@type" m
    , pShowType = read $ T.unpack $ txt "/presence/show/-" m
    , pStatus   = txt "/presence/status/-" m
    , pPriority = mread $ T.unpack $ txt "/presence/priority/-" m
    , pExt      = xtract id "/presence/*" m
    , pPurpose  = SIncoming
    }

instance StanzaDecoder 'IQ 'Incoming (Content Posn) where
  decodeStanza m = Just MkIQ { iqFrom   = mread $ T.unpack $ txt "/iq/@from" m
                            , iqTo      = mread $ T.unpack $ txt "/iq/@to" m
                            , iqId      = txt "/iq/@id" m
                            , iqType    = read $ T.unpack $ txt "/iq/@type" m
                            , iqBody    = [m]
                            , iqPurpose = SIncoming
                            }

