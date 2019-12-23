-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Print
-- Copyright   :  (c) Dmitry Astapov, 2006 ; pierre, 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An XMPP pretty-printing combinators
-- Ported from Text.HTML to HaXML combinatiors
--
-----------------------------------------------------------------------------

module Network.XMPP.Print
  ( -- Top-level rendering functions
    renderXmpp
  , putXmppLn
  , hPutXmpp
  , hPutNode
    -- XMPP primitives: tags
  , stream
  , streamEnd
    -- XMPP primitives: attributes
  , to
  ) where

import           System.IO
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import           Text.XML.HaXml        hiding (tag)
import           Text.XML.HaXml.Types  (Content)
import qualified Text.XML.HaXml.Pretty as P
import           Text.XML              (Node)
import Text.Blaze.Renderer.Text        (renderMarkup)
import Text.Blaze                      (toMarkup)
import Text.XML.HaXml.Posn
import Network.XMPP.UTF8
import Network.XMPP.Utils

-- | Convert the internal representation (built using HaXml combinators) into string, 
-- and print it out
putXmppLn :: Content Posn -> IO ()
putXmppLn = putStrLn . renderXmpp

-- | Convert the internal representation (built using HaXml combinators) into string, 
-- and print it to the specified Handle, without trailing newline
hPutXmpp :: Handle -> Content Posn -> IO ()
hPutXmpp h msg = 
  do let str = renderXmpp msg
     debugIO $ "Sending: " ++ str
     hPutStr h $ toUTF8 str

hPutNode :: Handle -> Node -> IO ()
hPutNode h n = do
  let str = T.unpack . TL.toStrict . renderMarkup . toMarkup $ n
  debugIO $ "Sending: " ++ str
  hPutStr h $ toUTF8 str

-- | Render HaXML combinators into string, hacked for XMPP
renderXmpp :: Content Posn -> String
renderXmpp theXml =
    case theXml of
      -- stupid hack for <stream:stream> and </stream:stream>
      xml@(CElem (Elem (N "stream:stream") _ _) _) ->
          (:) '<' $ takeWhile (/= '<') $ tail $ render $ P.content xml
      xml ->
          render $ P.content xml

---
--- XMPP construction combinators, based on the Text.Html
---

stream :: Show a => a -> String -> CFilter i
stream typ server =
  mkElemAttr "stream:stream"
    [ strAttr "xmlns:stream" "http://etherx.jabber.org/streams"
    , strAttr "xml:language" "en"
    , strAttr "version" "1.0"
    , strAttr "to" server
    , strAttr "xmlns" (show typ)
    ]
    [ mkElemAttr "" [] []  ]
-- TODO: to use hamlet here, we shoud be able to render non-closing tag like `<stream ...>`
--       but hamlet autho close tags and i see no ways to control it
-- head [xml|
--   <stream:stream
--     xmlns:stream="http://etherx.jabber.org/streams"
--     xml:language="en"
--     version="1.0"
--     to=#{T.pack server}
--     xmlns=#{T.pack (show typ)}
--   />
--

streamEnd :: CFilter i
streamEnd =
    mkElemAttr "/stream:stream" [] [ mkElemAttr "" [] [] ]

---
--- Predefined XMPP attributes
---
to :: String -> (String, CFilter i)
to = strAttr "to"

