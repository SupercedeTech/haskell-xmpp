{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Stream
-- Copyright   :  (c) Dmitry Astapov, 2006 ; pierre, 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An XMPP stream: means to create and use one
--
-----------------------------------------------------------------------------
module Network.XMPP.Stream
  ( XmppSendable(..)
  , Plugin(..)
  , startM, nextM, withNextM, selectM, xtractM, textractM, withSelectM
  , resetStreamHandle, getText, getText_
  , loopWithPlugins
  , getNextId
  , parse, parseM, waitAndProcess
  , withUUID
  ) where

import           Control.Monad                (void)
import           Control.Monad.State          (MonadState(..), gets, modify)
import           Control.Monad.Except         (runExceptT, throwError, lift)
import           Control.Monad.IO.Class       (MonadIO(..))
import           Control.Applicative          (Alternative, empty, pure)
import           System.IO                    (Handle, hGetContents)
import           Data.Text                    (Text, unpack, pack, intercalate)
import qualified Data.UUID.V4                 as UUID
import qualified Data.UUID                    as UUID
import           Data.Functor                 (($>))

import           Text.XML                     (Node)
import           Text.XML.HaXml.Lex           (xmlLex)
import           Text.XML.HaXml.Parse
import           Text.XML.HaXml.Posn          (Posn, noPos)
import           Text.XML.HaXml.Types
import qualified Text.XML.HaXml.Pretty        as P (content)
import           Text.XML.HaXml.Xtract.Parse  (xtract)
import           Text.ParserCombinators.Poly.State (onFail)

import           Network.XMPP.Print           (hPutNode, hPutXmpp)
import           Network.XMPP.Utils
import           Network.XMPP.Types
import           Network.XMPP.UTF8
import           Network.XMPP.XML
import           Network.XMPP.Stanza

-- Main 'workhorses' for Stream are 'xmppSend', 'nextM', 'peekM' and 'selectM':
-- | Sends message into Stream
class XmppSendable a where
  xmppSend :: a -> XmppMonad ()

instance XmppSendable Node where
  xmppSend node = do
    h <- gets handle
    liftIO $ hPutNode h node

instance XmppSendable (Content Posn) where
  xmppSend content = do
    h <- gets handle
    liftIO $ hPutXmpp h content

instance XmppSendable (Stanza t 'Outgoing e) where
  xmppSend s = xmppSend (encodeStanza s :: Node)

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
parseM :: FromXML e => XmppMonad (Either Text (SomeStanza e))
parseM = (fmap parse <$> nextM) >>= \case
  Right m -> maybe parseM (pure . Right) m
  Left  e -> pure $ Left e

-- | Skips all messages, that will return result `Nothing` from computation
-- | In other words - waits for appropriate message, defined by predicate
waitAndProcess :: FromXML e => (SomeStanza e -> Maybe a) -> XmppMonad (Either Text a)
waitAndProcess compute = parseM >>= \case
  Right m   -> maybe (waitAndProcess compute) (pure . Right) $ compute m
  Left  err -> pure $ Left err

withUUID :: MonadIO m => (UUID.UUID -> Stanza t p e) -> m (Stanza t p e)
withUUID setUUID = setUUID <$> liftIO UUID.nextRandom

-- | Selects next messages from stream
nextM :: XmppMonad (Either Text (Content Posn))
nextM = do
  ls <- gets lexemes
  let (elem, rest) = xmlParseWith element ls
  case elem of
    Left  err ->
      let log = ["Failed to parse next element:", pack (show err), ", msg:", pack (show ls)]
      in pure $ Left $ intercalate " " log
    Right e   -> do
      let msg = CElem e noPos
      debug $ "nextM: Got element: " ++ show (P.content msg)
      modify (\stream -> stream { lexemes = rest })
      pure $ Right msg

-- | Selects next message matching predicate
selectM :: (Content Posn -> Bool) -> XmppMonad (Either Text (Content Posn))
selectM p = runExceptT $ do
  m <- lift nextM >>= either throwError pure
  if p m then pure m else throwError "Failed to select message"

-- | Pass in xtract query, return query result from the first message where it returns non-empty results
xtractM :: Text -> XmppMonad [Content Posn]
xtractM q =do
  eim <- selectM (not . null . xtract id (unpack q))
  case eim of
    Right m -> pure $ xtract id (unpack q) m
    Left _e -> pure [] -- TODO

textractM :: Text -> XmppMonad Text
textractM q = do
  res <- xtractM q
  pure $ case res of
    [] -> ""
    x  -> getText_ x

withNextM :: (Content Posn -> b) -> XmppMonad (Either Text b)
withNextM compute = fmap compute <$> nextM

withSelectM :: (Content Posn -> Bool) -> (Content Posn -> b) -> XmppMonad (Either Text b)
withSelectM predicate compute =
  selectM predicate >>= either (pure . Left) (pure . Right . compute)

-- | startM is a special accessor case, since it has to retrieve only opening tag of the '<stream>' message,
-- which encloses the whole XMPP stream. That's why it does it's own parsing, and does not rely on 'nextM'
startM :: XmppMonad (Either Text [Attribute])
startM = do
  (starter, rest) <- xmlParseWith streamStart <$> gets lexemes
  case starter of
    Left e -> pure $ Left $ pack e
    Right (ElemTag (N "stream:stream") attrs) ->
      modify (\stream -> stream { lexemes = rest }) $> Right attrs
    Right _ ->
      pure $ Left "Unexpected element at the beginning of XMPP stream!"
 where
  streamStart = void processinginstruction `onFail` return () >> elemOpenTag

-- | Replaces contents of the Stream with the contents coming from given handle.
resetStreamHandle :: (MonadIO m, MonadState Stream m) => Handle -> m ()
resetStreamHandle h =
  do c <- liftIO $ hGetContents h
     modify (\stream -> stream { handle=h , lexemes = xmlLex "stream" (fromUTF8 c) })

-------------------------------
-- Basic plugin support
data Plugin
    = Plugin
    { trigger :: String
    , body    :: Content Posn -> XmppMonad ()
    }

loopWithPlugins :: [Plugin] -> XmppMonad (Either Text ())
loopWithPlugins ps =
  let loop = nextM >>= \case
        Right m -> do
          let notEmpty p = not $ null $ xtract id (trigger p) m
          sequence_ [ body p m | p <- ps, notEmpty p ] >> loop
        Left _e -> loop
  in  loop

getNextId :: XmppMonad Int
getNextId = do
  i <- gets idx
  modify (\stream -> stream { idx = i + 1 })
  return i
