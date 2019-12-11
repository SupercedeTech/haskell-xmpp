{-# LANGUAGE FlexibleContexts #-}

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
  ( 
    out
  , startM
  , nextM    
  , withNextM
  , selectM
  , xtractM
  , textractM
  , withSelectM
  , withNewStream
  , withStream
  , resetStreamHandle
  , getText
  , getText_
  , loopWithPlugins
  , Plugin(..)
  , getNextId
  , lookupAttr
  , newStream
  ) where

import Control.Monad.State
import System.IO
import Text.ParserCombinators.Poly.State (onFail)
import Text.XML.HaXml.Lex (xmlLex)
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (noPos)
import Text.XML.HaXml.Types
import qualified Text.XML.HaXml.Pretty as P (content)
import Text.XML.HaXml.Xtract.Parse (xtract)

import Network.XMPP.Print
import Network.XMPP.Utils
import Network.XMPP.Types
import Network.XMPP.UTF8

-- For a definition of 'Stream' see Network.XMPP.Types.

-- In the beginning, all Stream buffers are empty, and by default it is bound to stdin.
-- Functions like 'openStreamTo' or 'openStreamViaProxyTo' could override this.
newStream :: Stream 
newStream = Stream { handle=stdin, idx=0, lexemes=[] }

-- Main 'workhorses' for Stream are 'out', 'nextM', 'peekM' and 'selectM':
-- | Sends message into Stream
out :: XmppMessage -> XmppStateT ()
out xmpp = do h <- gets handle
              liftIO $ hPutXmpp h xmpp

-- | Selects next messages from stream
nextM :: XmppStateT XmppMessage
nextM = 
  do ls <- gets lexemes
     let (elem, rest) = xmlParseWith element ls
     case elem of 
          (Left err) -> error $ "Failed to parse next element: " ++ show err
          (Right e) -> do let msg = CElem e noPos
                          debug $ "nextM: Got element: " ++ show (P.content msg)
                          modify (\stream -> stream { lexemes = rest } )
                          return msg

-- | Selects next message matching predicate
selectM :: (XmppMessage -> Bool) -> XmppStateT XmppMessage
selectM p =
  do m <- nextM
     if p m then return m
            else error "Failed to select message"

-- | Pass in xtract query, return query result from the first message where it returns non-empty results
xtractM :: String ->  XmppStateT [XmppMessage]
xtractM q = 
  do m <- selectM (not . null . (xtract id q))
     return $ xtract id q m

textractM :: String -> XmppStateT String
textractM q =  do res <- xtractM q
                  return $ case res of
                                [] -> ""
                                x  -> getText_ x

-- All accessor functions have a convenience wrappers:
withM acc f = do m <- acc; return (f m)
withNextM = withM nextM
withSelectM p = withM (selectM p)

-- | startM is a special accessor case, since it has to retrieve only opening tag of the '<stream>' message,
-- which encloses the whole XMPP stream. That's why it does it's own parsing, and does not rely on 'nextM'
startM :: XmppStateT [Attribute]
startM =
  do ls <- gets lexemes
     let (starter, rest) = xmlParseWith streamStart ls
     case starter of
          Left e -> error e
          Right (ElemTag (N "stream:stream") attrs) -> do modify (\stream -> stream { lexemes=rest })
                                                          return attrs
          Right _ -> error $ "Unexpected element at the beginning of XMPP stream!"
  where
  streamStart = do ( processinginstruction >> return () ) `onFail` return ()
                   elemOpenTag

-- | Convenience wrappers which allow for nicer code like:
--  withNewStream $ do ...
withNewStream :: XmppStateT a -> IO (a, Stream)
withNewStream f = runStateT (runXmppStateT f) newStream

withStream :: Stream -> XmppStateT a -> IO (a, Stream)
withStream s f = runStateT (runXmppStateT f) s

-- | Replaces contents of the Stream with the contents
--   coming from given handle.
resetStreamHandle h =
  do c <- liftIO $ hGetContents h
     modify (\stream -> stream { handle=h , lexemes = xmlLex "stream" (fromUTF8 c) })

-------------------------------
-- Basic plugin support
data Plugin = Plugin { trigger::String, body::(XmppMessage -> XmppStateT ()) }

loopWithPlugins :: [Plugin] -> XmppStateT ()
loopWithPlugins ps =
  let loop = do m <- nextM
                sequence_ [ (body p) m | p <- ps, not (null (xtract id (trigger p) m)) ]
                loop
      in loop

getNextId :: XmppStateT Int
getNextId =
  do i <- gets idx
     modify (\stream -> stream { idx = i+1 })
     return i

lookupAttr :: String -> [Attribute] -> Maybe String
lookupAttr k lst =
  do x <- lookup (N k) lst
     case x of
            AttValue [Left str] -> Just str
            AttValue _          -> Nothing

