-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Helpers
-- Copyright   :  (c) Dmitry Astapov, 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Various "connection helpers" that let user obtain a handle to pass to 'initiateStream'
--
-----------------------------------------------------------------------------

module Network.XMPP.Helpers 
  ( connectViaHttpProxy
  , connectViaTcp
  , openStreamFile
  ) where

import System.IO (Handle, hPutStrLn, hPutStr, hGetLine, openFile, IOMode(..))
import Network (connectTo, PortID(..) )
import Network.Socket
import Network.BSD
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Trans (liftIO)
import Network.XMPP.Utils

-- | Connect to XMPP server on specified host \/ port
connectViaTcp :: String -- ^ Server (hostname) to connect to
             -> Int     -- ^ Port to connect to
             -> IO Handle
connectViaTcp server port = do
  host <- getHostByName server
  sock <- socket AF_INET Stream 0
  setSocketOption sock KeepAlive 1
  connect sock (SockAddrInet (fromIntegral port) (head $ hostAddresses host))
  socketToHandle sock ReadWriteMode 

-- | Connect to XMPP server on specified host \/ port
--  via HTTP 1.0 proxy
connectViaHttpProxy proxy_server proxy_port server port =
  do h <- connectTo proxy_server (PortNumber (fromIntegral proxy_port))
     hPutStrLn h $ unlines [ concat ["CONNECT ",server,":",show port," HTTP/1.0"]
                           , "Connection: Keep-Alive" ]
     dropHeaders h
     liftIO $ forkIO $ pinger h
     return h
  where dropHeaders h = do l <- hGetLine h
                           debugIO $ "Got: " ++ l
                           if words l /= [] then dropHeaders h
                                            else return ()
        pinger h = hPutStr h " " >> threadDelay (30 * 10^6) >> pinger h

-- | Open file with pre-captured server-to-client XMPP stream. For debugging
openStreamFile fname = openFile fname ReadMode