{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader

import Network.XMPP.Stream
import Network.XMPP.Types
import Network.XMPP.Utils
import Network.XMPP.XML

import System.IO

data Thread e = Thread { inCh :: TChan (SomeStanza e)
                     , outCh :: TChan (SomeStanza e)
                     }

type XmppThreadT a e = ReaderT (Thread e) IO a

-- Two streams: input and output. Threads read from input stream and write to output stream.
-- | Runs thread in XmppState monad
runThreaded :: FromXML e => XmppThreadT () e -> XmppMonad IO ()
runThreaded a = do
  in' <- liftIO $ atomically newTChan
  out' <- liftIO $ atomically newTChan
  void $ liftIO $ forkIO $ runReaderT a $ Thread in' out'
  s <- get
  void $ liftIO $ forkIO $ loopWrite s out'
  void $ liftIO $ forkIO $ connPersist $ handle s
  loopRead in'
    where 
      loopRead in' = loop $ do
        eiMsg <- parseM
        case eiMsg of
          Right m -> liftIO . atomically . writeTChan in' $ m
          Left err -> liftIO $ print $ "Error in thread: " <> show err
      loopWrite s out' =
        void $ runXmppMonad $ do
          put s
          loop $ do
            st <- liftIO $ atomically $ readTChan out'
            case st of
                SomeStanza stnz@MkMessage{ mPurpose = SOutgoing }  -> xmppSend stnz
                SomeStanza stnz@MkPresence{ pPurpose = SOutgoing } -> xmppSend stnz
                SomeStanza stnz@MkIQ{ iqPurpose = SOutgoing }      -> xmppSend stnz
                _                            -> pure () -- Won't happen, but we gotta make compiler happy
      loop = sequence_ . repeat

readChanS :: XmppThreadT (SomeStanza e) e
readChanS =
  asks inCh >>= liftIO . atomically . readTChan

writeChanS :: SomeStanza e -> XmppThreadT () e
writeChanS a = 
  void $ asks outCh >>= liftIO . atomically . flip writeTChan a 

-- | Runs specified action in parallel
withNewThread :: XmppThreadT () e -> XmppThreadT ThreadId e
withNewThread a = do
  newin <- asks inCh >>= liftIO . atomically . dupTChan
  asks outCh >>= liftIO . forkIO . runReaderT a . Thread newin

-- | Turns action into infinite loop
loop :: XmppThreadT () e -> XmppThreadT () e
loop a = a >> loop a

waitFor :: (SomeStanza e -> Bool) -> XmppThreadT (SomeStanza e) e
waitFor f = do
  s <- readChanS
  if f s then return s else waitFor f

connPersist :: Handle -> IO ()
connPersist h = do
  hPutStr h " "
  debugIO "<space added>"
  threadDelay 30000000
  connPersist h
