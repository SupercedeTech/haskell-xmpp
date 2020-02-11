{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Concurrent
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  k.pierre.k@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Concurrent actions over single IO channel
--
-----------------------------------------------------------------------------
module Network.XMPP.Concurrent
  ( Thread
  , XmppThreadT
  , runThreaded
  , readChanS
  , writeChanS
  , withNewThread
  , loop
  , waitFor
  ) where

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader

import Network.XMPP.Stream
import Network.XMPP.Types
import Network.XMPP.Utils
import Network.XMPP.XML
import UnliftIO.Async          (Async, async)
import UnliftIO                (TChan, MonadUnliftIO, atomically, newTChan,
                                writeTChan, readTChan, dupTChan)

import System.IO

data Thread e = Thread
  { tInCh  :: TChan (Either XmppError (SomeStanza e))
  , tOutCh :: TChan (SomeStanza ())
  }

type XmppThreadT m a e = ReaderT (Thread e) m a

instance MonadIO m => XmppSendable (ReaderT (Thread e) m) (Stanza t 'Outgoing ()) where
  xmppSend = writeChanS . SomeStanza

-- Two streams: input and output. Threads read from input stream and write to output stream.
-- | Runs thread in XmppState monad
runThreaded
  :: (FromXML e, MonadIO m, MonadUnliftIO m)
  => XmppThreadT m () e
  -> XmppMonad m ()
runThreaded action = do
  (in', out')  <- atomically $ (,) <$> newTChan <*> newTChan
  s@Stream{..} <- get
  void $ lift $
    async (runReaderT action $ Thread in' out') >>
    async (void $ async $ runXmppMonad' s $ loopWrite out') >>
    async (connPersist handle)
  loopRead in'
 where
  loopRead in' = do
    msg <- parseM
    atomically $ writeTChan in' msg
    case msg of
      Left StreamClosedError -> pure ()
      Left RanOutOfInput     -> pure ()
      _                      -> loopRead in'
  loopWrite :: MonadIO m => TChan (SomeStanza e) -> XmppMonad m ()
  loopWrite out'= do
    liftIO (atomically $ readTChan out') >>= \case
      SomeStanza stnz@MkMessage { mPurpose = SOutgoing } -> xmppSend stnz
      SomeStanza stnz@MkPresence { pPurpose = SOutgoing } -> xmppSend stnz
      SomeStanza stnz@MkIQ { iqPurpose = SOutgoing } -> xmppSend stnz
      _ -> pure () -- Won't happen, but we gotta make compiler happy
    loopWrite out'

readChanS :: MonadIO m => XmppThreadT m (Either XmppError (SomeStanza e)) e
readChanS = asks tInCh >>= liftIO . atomically . readTChan

writeChanS :: MonadIO m => SomeStanza () -> XmppThreadT m () e
writeChanS a = void $ asks tOutCh >>= liftIO . atomically . flip writeTChan a

-- | Runs specified action in parallel
withNewThread
  :: (MonadIO m, MonadUnliftIO m)
  => XmppThreadT m () e
  -> XmppThreadT m (Async ()) e
withNewThread a = do
  newin <- asks tInCh >>= liftIO . atomically . dupTChan
  asks tOutCh >>= lift . async . runReaderT a . Thread newin

-- | Turns action into infinite loop
loop :: MonadIO m => XmppThreadT m () e -> XmppThreadT m () e
loop a = a >> loop a

waitFor
  :: MonadIO m
  => (Either XmppError (SomeStanza e) -> Bool)
  -> XmppThreadT m (Either XmppError (SomeStanza e)) e
waitFor f = do
  s <- readChanS
  if f s then return s else waitFor f

connPersist :: MonadIO m => Handle -> m ()
connPersist h = do
  liftIO $ hPutStr h " "
  liftIO $ debugIO "<space added>"
  liftIO $ threadDelay 30000000
  connPersist h
