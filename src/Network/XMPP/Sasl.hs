-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Sasl
-- Copyright   :  (c) Dmitry Astapov, 2006 ; pierre, 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- SASL Authentication for XMPP
--
-----------------------------------------------------------------------------

module Network.XMPP.Sasl
  ( saslAuth
  ) where

import Control.Monad (when,unless)
import Control.Monad.State (liftIO)
import Data.Char (chr,ord)
import Data.List
import Numeric (showHex)
import System.Random (newStdGen, randoms)
import Text.XML.HaXml.Combinators hiding (when)
import Text.XML.HaXml.Posn         (Posn, noPos)
import Text.XML.HaXml              (Element(Elem), mkElemAttr, Content (CElem),
                                    QName(N))


import Network.XMPP.Base64 as B64
import Network.XMPP.MD5 
import Network.XMPP.Print
import Network.XMPP.Stream
import Network.XMPP.Types
import Network.XMPP.Utils

-- | Perform authentication over already-open channel
saslAuth :: [String] -- ^ List of auth mechanism available from server, currently only "DIGEST-MD5" is supported
         -> String   -- ^ Server we are connectint to (hostname)
         -> String   -- ^ Username to connect as
         -> String   -- ^ Password
         -> XmppMonad ()
saslAuth mechanisms server username password
  | "DIGEST-MD5" `elem` mechanisms = saslDigest server username password
  | otherwise                      = error $ "Dont know how to do auth! Available mechanisms are: " ++ show mechanisms


saslDigest server username password = do
    out $ head (auth noelem)
    ch_text <- withNextM getChallenge
    resp <- liftIO $ saslDigestResponse ch_text username server password
    out $ head (response resp noelem)
    m <- nextM
    when (not $ null $ tag "failure" $ m) $
        (error "Auth failure") -- TODO Oo! ehrm, no!
    let chl_text = getChallenge m
    saslDigestRspAuth chl_text
    out $ head (sndResponse noelem)
    m <- nextM
    unless (not $ null $ tag "success" m) $
        (error "Auth failed") -- TODO Oo! ehrm, no!
    where
        noelem = CElem (Elem (N "root") [] []) noPos
        auth = mkElemAttr "auth"
                  [ strAttr "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl"
                  , strAttr "mechanism" "DIGEST-MD5"
                  ]
                  []
        response resp = mkElemAttr "response"
                  [ strAttr "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl" ]
                  [ literal $ resp ]
        sndResponse = mkElemAttr "response"
                  [ strAttr "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl" ]
                  []
        getChallenge c =
            case (tag "challenge" /> txt) c of
                [] -> error "Wheres challenge?"
                x  -> getText_ x

saslDigestResponse chl username server password =
  let pairs = get_pairs $ B64.decode chl
      Just qop = lookup "qop" pairs
      Just nonce = lookup "nonce" pairs
      nc = "00000001"
      digest_uri ="xmpp/" ++ server
      realm = server 
      in do cnonce <- make_cnonce
            let a1 = semi_sep [ md5raw (Str (semi_sep [username,realm,password])), nonce, cnonce]
            let a2 = "AUTHENTICATE:" ++ digest_uri
            let t  = semi_sep [ md5s (Str a1), nonce, nc, cnonce, qop, md5s (Str a2) ]
            let response = md5s (Str t)
            let resp = concat [ "username=", show username
                              , ",realm=", show realm
                              , ",nonce=", show nonce
                              , ",cnonce=", show cnonce
                              , ",nc=", nc
                              , ",qop=", qop
                              , ",digest-uri=", show digest_uri
                              , ",response=", response
                              ]
            return $ B64.encode resp
  where
  md5raw = map (chr . read . ("0x"++ ) . take 2) . takeWhile (not.null) . iterate (drop 2) . md5s
  hexa str = foldr showHex "" $ map ord str
  semi_sep = concat . intersperse ":"
  make_cnonce = do g <- newStdGen
                   return $ hexa $ map (chr.(`mod` 256)) $ take 8 $ randoms g

-- | Split aaa=bbb,foo="bar" into [("aaa","bbb"),("foo","bar")]
get_pairs str = 
  let chunks = map (takeWhile (/=',')) $ takeWhile (not.null) $ iterate (drop 1 . dropWhile (/=',')) str
      (keys, values) = unzip $ map (break (=='=')) chunks
      in zip keys $ map trim values
  where
  -- | Trim leading '=' and surrounding quotes (if any) from value
  trim str = case dropWhile (=='=') str of
                  x@('\"':_) -> read x
                  x          -> x

saslDigestRspAuth chl =
  let pairs = get_pairs $ B64.decode chl
      in case lookup "rspauth" pairs of
              Just _ -> return ()
              Nothing -> error "NO rspauth in SASL digest rspauth!"
