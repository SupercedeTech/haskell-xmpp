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
import Control.Monad (void, when)

import Network.BSD (getHostByName, hostAddresses)
import qualified Data.Text as T

import Network.Socket
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Trans (liftIO)
import Network.XMPP.Utils

-- | Connect to XMPP server on specified host \/ port
connectViaTcp :: T.Text -- ^ Server (hostname) to connect to
              -> Int     -- ^ Port to connect to
              -> IO Handle
connectViaTcp server port = do
  host <- getHostByName $ T.unpack server
  sock <- socket AF_INET Stream 0
  setSocketOption sock KeepAlive 1
  let sockAddress = SockAddrInet (fromIntegral port) $ head $ hostAddresses host
  connect sock sockAddress
  socketToHandle sock ReadWriteMode

-- | Connect to XMPP server on specified host \/ port
--  via HTTP 1.0 proxy
connectViaHttpProxy :: Show a => HostName -> Integer -> T.Text -> a -> IO Handle
connectViaHttpProxy proxyServer proxyPort server port = do
  let hints = defaultHints
        { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
        , addrSocketType = Stream
        }
  addr:_ <- getAddrInfo (Just hints) (Just proxyServer) (Just $ show proxyPort)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  h <- socketToHandle sock ReadWriteMode
  hPutStrLn h $ unlines
    [ concat ["CONNECT ", T.unpack server, ":", show port, " HTTP/1.0"]
    , "Connection: Keep-Alive"
    ]
  dropHeaders h
  void $ liftIO $ forkIO $ pinger h
  return h
 where
  dropHeaders h = do
    l <- hGetLine h
    debugIO $ "Got: " ++ l
    when (words l /= []) $ dropHeaders h
  pinger h = hPutStr h " " >> threadDelay (30 * (10 ^ (6 :: Int))) >> pinger h

-- | Open file with pre-captured server-to-client XMPP stream. For debugging
openStreamFile :: FilePath -> IO Handle
openStreamFile fname = openFile fname ReadMode
