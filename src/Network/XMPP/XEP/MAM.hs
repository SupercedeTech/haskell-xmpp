{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.XMPP.XEP.MAM
  ( messageArchiveQueryStanza
  , defMamQuery
  , MAMQuery(..)
  , MAMPayload(..)
  ) where

import qualified Data.UUID              as UUID
import           Data.Time              (UTCTime)
import           Data.Text              (Text, pack)
import           Data.Maybe             (catMaybes)

import           Text.Hamlet.XML        (xml)

import           Network.XMPP.Types
import           Network.XMPP.XML       (ToXML(..), FromXML(..), matchPatterns,
                                         txtpat, mread)
import           Network.XMPP.XEP.Form  (XmppForm(..), XmppField(..))

--
-- Messaging archives management extenstion
-- https://xmpp.org/extensions/xep-0313.html#query
--

messageArchiveQueryStanza :: MAMQuery -> UUID.UUID -> Stanza 'IQ 'Outgoing ()
messageArchiveQueryStanza MAMQuery {..} uuid =
  let form = XmppForm $ catMaybes
        [ Just $ HiddenField "FORM_TYPE" "urn:xmpp:mam:2"
        , SingleTextField "with" . pack . show <$> mqWith
        , SingleTextField "start" . pack . show <$> mqStart
        , SingleTextField "end" . pack . show <$> mqEnd
        ]
  in  MkIQ { iqFrom = Nothing
           , iqTo   = SomeJID <$> mqRoom
           , iqId   = UUID.toText uuid
           , iqType = Set
           , iqPurpose = SOutgoing
           , iqBody = [xml| 
              <query xmlns="urn:xmpp:mam:2">
                ^{encodeXml form}

                <set xmlns="http://jabber.org/protocol/rsm">
                  <max>#{pack $ show mqLimit}
                  $maybe afterId <- mqAfter
                    <after>#{afterId}

                  $if mqFromLatest
                    <before>
                      $maybe beforeId <- mqBefore
                        #{beforeId}
                  $else
                    $maybe beforeId <- mqBefore
                      <before>#{beforeId}
            |]
          }

data MAMQuery = MAMQuery
  { mqStart :: Maybe UTCTime
  , mqEnd   :: Maybe UTCTime
  , mqWith  :: Maybe (JID 'Node)
  , mqRoom  :: Maybe (JID 'Node)
  , mqLimit :: Int
  , mqAfter :: Maybe Text
  , mqBefore :: Maybe Text
  , mqFromLatest :: Bool
  } deriving (Show)

defMamQuery :: MAMQuery
defMamQuery = MAMQuery
  { mqStart = Nothing
  , mqEnd   = Nothing
  , mqWith  = Nothing
  , mqRoom  = Nothing
  , mqLimit = 10
  , mqAfter = Nothing
  , mqBefore = Nothing
  , mqFromLatest = False
  }

data MAMPayload = MAMFinalPayload
  { mComplete :: Bool
  , mLast     :: Text
  , mFirst    :: Text
  , mFirstIdx :: Text
  , mCount    :: Int
  } deriving (Show)

instance FromXML MAMPayload where
  decodeXml m
    | matchPatterns m ["/fin/@complete", "/fin/set/count"]
    = MAMFinalPayload
      <$> decodeBool (txtpat "/fin/@complete" m)
      <*> Just (txtpat "/fin/set/last/-" m)
      <*> Just (txtpat "/fin/set/first/-" m)
      <*> Just (txtpat "/fin/set/first@index" m)
      <*> mread (txtpat "/fin/set/count/-" m)
    | otherwise
    = Nothing
    where
      decodeBool "true"  = Just True
      decodeBool "false" = Just False
      decodeBool _       = Nothing
