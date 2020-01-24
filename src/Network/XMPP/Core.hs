{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Core
-- Copyright   :  (c) Dmitry Astapov, 2006 ; pierre, 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of XMPP Core Protocol (RFC 3920)
--
-----------------------------------------------------------------------------

module Network.XMPP.Core
  ( initStream
  , closeStream
  ) where

import Control.Monad        (void)
import System.IO            (Handle, hSetBuffering, BufferMode(..))
import Control.Monad.Except (throwError, runExceptT, lift)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Text            (Text, unpack)
import Text.Hamlet.XML      (xml)

import Network.XMPP.Utils   (debug)
import Network.XMPP.Sasl    (saslAuth)
import Network.XMPP.IQ      (iqSend)
import Network.XMPP.Print   (stream, streamEnd)
import Network.XMPP.XML     (noelem, lookupAttr, getText)
import Network.XMPP.Types   (Server, Username, Password, Resource, XmppMonad,
                             JID(..), JIDQualification(..), StreamType(..),
                             IQType(..))
import Network.XMPP.Stream  (resetStreamHandle, XmppSendable(..),
                             xtractM, textractM, startM)

-- | Open connection to specified server and return `Stream' coming from it
initStream :: MonadIO m => Handle
               -> Server -- ^ Server (hostname) we are connecting to
               -> Username -- ^ Username to use
               -> Password -- ^ Password to use
               -> Resource -- ^ Resource to use
               -> XmppMonad m (Either Text (JID 'NodeResource))
initStream h server username password resrc = runExceptT $
  do liftIO $ hSetBuffering h NoBuffering
     resetStreamHandle h
     lift $ xmppSend $ head $ stream Client server noelem
     attrs <- lift startM >>= either throwError pure

     case lookupAttr "version" attrs of
        Just "1.0" -> return ()
        -- TODO: JEP 0078 in case of absent of version we wont process stream:features
        Nothing -> throwError "No version"
        _ -> throwError "unknown version"
     
     lift $ debug "Stream started"
     --debug $ "Observing: " ++ render (P.content m)
     m <- lift $ xtractM "/stream:features/mechanisms/mechanism/-"
     let mechs = getText <$> m
     lift $ debug $ "Mechanisms: " ++ show mechs

     -- Handle the authentication
     lift (saslAuth mechs server username password) >>= either throwError pure

     lift $ xmppSend $ head $ stream Client server noelem

     void $ lift startM >>= either throwError pure

     -- Bind this session to resource
     lift $ void $ xtractM "/stream:features/bind" -- `catch` (fail "Binding is not proposed")

     lift $ iqSend "bind1" Set 
                  [xml|
                    <bind xmlns="urn:ietf:params:xml:ns:xmpp-bind">
                      <resource>#{resrc}
                  |]
                
     my_jid <- lift $ textractM "/iq[@type='result' & @id='bind1']/bind/jid/-"

     lift $ iqSend "session1" Set 
                [xml| <session xmlns="urn:ietf:params:xml:ns:xmpp-session"> |]

     lift $ void $ xtractM "/iq[@type='result' & @id='session1']" -- (error "Session binding failed")

     return $ read $ unpack my_jid

closeStream :: MonadIO m => XmppMonad m ()
closeStream = xmppSend $ head $ streamEnd noelem
