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
    -- XMPP primitives: tags
  , stream
  , streamEnd
    -- XMPP primitives: attributes
  , to
  , xmlns
  , xmllang
  , language
  , stream_version
  , mechanism
  , type_
  , id_
  , from
  ) where

import           System.IO
import           Text.XML.HaXml        hiding (tag)
import           Text.XML.HaXml.Types  (Content)
import           Text.XML.HaXml.Posn   (Posn)
import qualified Text.XML.HaXml.Pretty as P
    
import Network.XMPP.UTF8
import Network.XMPP.Types
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

stream :: (Show a) => a -> String -> CFilter i
stream typ server =
    mkElemAttr "stream:stream"
            [ strAttr "xmlns:stream" "http://etherx.jabber.org/streams"
            , strAttr "xml:language" "en"
            , strAttr "version" "1.0"
            , strAttr "to" server
            , xmlns (show typ)
            ]
            [ itag "" [] ]  

streamEnd :: CFilter i
streamEnd =
    mkElemAttr "/stream:stream" [] [ itag "" [] ]

---
--- Predefined XMPP attributes
---
to :: String -> (String, CFilter i)
to = strAttr "to"

xmlns :: String -> (String, CFilter i)
xmlns = strAttr "xmlns"

language :: String -> (String, CFilter i)
language = strAttr "xml:language" 

xmllang :: String -> (String, CFilter i)
xmllang = strAttr "xml:lang" 

stream_version :: String -> (String, CFilter i)
stream_version = strAttr "version"

mechanism :: String -> (String, CFilter i)
mechanism = strAttr "mechanism"

type_ :: String -> (String, CFilter i)
type_ = strAttr "type"

id_ :: String -> (String, CFilter i)
id_ = strAttr "id"

from :: String -> (String, CFilter i)
from = strAttr "from"

