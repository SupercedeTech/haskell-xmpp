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

import Network.XMPP.Stream
import Network.XMPP.Stanza
import Network.XMPP.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State 
import Control.Monad.Reader 
import Network.XMPP.Utils

import System.IO

data Thread = Thread { inCh :: TChan SomeStanza
                     , outCh :: TChan SomeStanza
                     }

type XmppThreadT a = ReaderT Thread IO a
       
-- Two streams: input and output. Threads read from input stream and write to output stream.
-- | Runs thread in XmppState monad
runThreaded :: XmppThreadT () -> XmppStateT ()
runThreaded a = do
  in' <- liftIO $ atomically $ newTChan
  out' <- liftIO $ atomically $ newTChan
  liftIO $ forkIO $ runReaderT a (Thread in' out')
  s <- get
  liftIO $ forkIO $ loopWrite s out'
  liftIO $ forkIO $ connPersist (handle s)
  loopRead in' 
    where 
      loopRead in' = loop $ 
        parseM >>= liftIO . atomically . writeTChan in'
      loopWrite s out' =
        void $ withNewStream $ do
          put s
          loop $ do
            st <- liftIO $ atomically $ readTChan out'
            case st of
                SomeStanza stnz -> outStanza stnz
      loop = sequence_ . repeat
       
readChanS :: XmppThreadT SomeStanza
readChanS =
  asks inCh >>= liftIO . atomically . readTChan

writeChanS :: SomeStanza -> XmppThreadT ()
writeChanS a = 
  void $ asks outCh >>= liftIO . atomically . flip writeTChan a 

-- | Runs specified action in parallel
withNewThread :: XmppThreadT () -> XmppThreadT ThreadId
withNewThread a = do
  newin <- asks inCh >>= liftIO . atomically . dupTChan
  asks outCh >>= liftIO . forkIO . runReaderT a . Thread newin

-- | Turns action into infinite loop
loop :: XmppThreadT () -> XmppThreadT ()
loop a = do
  a
  loop a

waitFor :: (SomeStanza -> Bool) -> XmppThreadT SomeStanza
waitFor f = do
  s <- readChanS
  if (f s) then 
    return s
    else do
      waitFor f

connPersist :: Handle -> IO ()
connPersist h = do
  hPutStr h " "
  debugIO "<space added>"
  threadDelay 30000000
  connPersist h
    
