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
  ( initiateStream
  ) where

import Control.Monad.State
import System.IO

import qualified Data.Text as T
import Text.Hamlet.XML      (xml)
import Text.XML.HaXml.Types (Content(..), Element(..), QName(..))
import Text.XML.HaXml.Posn  (noPos, Posn)
import Network.XMPP.Sasl    (saslAuth)
import Network.XMPP.Print
import Network.XMPP.Stream
import Network.XMPP.Types
import Network.XMPP.IQ
import Network.XMPP.Utils

-- | Open connection to specified server and return `Stream' coming from it
initiateStream :: Handle
               -> Server -- ^ Server (hostname) we are connecting to
               -> Username -- ^ Username to use
               -> Password -- ^ Password to use
               -> Resource -- ^ Resource to use
               -> XmppMonad (JID 'NodeResource)
initiateStream h server username password resrc =
  do liftIO $ hSetBuffering h NoBuffering
     resetStreamHandle h
     outPosn $ head $ stream Client server noelem
     attrs <- startM
     case lookupAttr "version" attrs of
        Just "1.0" -> return ()
        Nothing ->
          -- TODO: JEP 0078
          -- in case of absent of version we wont process stream:features
          error "No version"
        _ -> error "unknown version"
     
     debug "Stream started"
     --debug $ "Observing: " ++ render (P.content m)
     m <- xtractM "/stream:features/mechanisms/mechanism/-"
     let mechs = getText <$> m
     debug $ "Mechanisms: " ++ show mechs

     -- Handle the authentication
     saslAuth mechs server username password

     outPosn $ head $ stream Client server noelem

     void startM

     -- Bind this session to resource
     void $ xtractM "/stream:features/bind" -- `catch` (fail "Binding is not proposed")

     iqSend "bind1" Set 
                  [xml|
                    <bind xmlns="urn:ietf:params:xml:ns:xmpp-bind">
                      <resource>#{resrc}
                  |]
                
     my_jid <- textractM "/iq[@type='result' & @id='bind1']/bind/jid/-"

     iqSend "session1" Set 
                [xml| <session xmlns="urn:ietf:params:xml:ns:xmpp-session"> |]

     void $ xtractM "/iq[@type='result' & @id='session1']" -- (error "Session binding failed")

     return (read $ T.unpack my_jid)

noelem :: Content Posn
noelem = CElem (Elem (N "root") [] []) noPos