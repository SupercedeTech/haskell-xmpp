{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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

import           Control.Monad                     (unless, join)
import           Control.Monad.IO.Class
import           Control.Monad.Except              (throwError, runExceptT,
                                                    liftIO, lift, ExceptT(..))
import           Data.Char                         (chr, ord)
import           Data.List                         (intercalate)
import qualified Data.Text                         as T
import           Numeric                           (showHex)
import           System.Random                     (newStdGen, randoms)
import           Text.XML.HaXml.Combinators hiding (when)
import           Text.Hamlet.XML

import qualified Network.XMPP.Base64 as B64
import qualified Network.XMPP.MD5    as MD5
import           Network.XMPP.Stream
import           Network.XMPP.Types

-- | Perform authentication over already-open channel
saslAuth :: MonadIO m
         => [T.Text] -- ^ List of auth mechanism available from server, currently only "DIGEST-MD5" is supported
         -> T.Text   -- ^ Server we are connectint to (hostname)
         -> T.Text   -- ^ Username to connect as
         -> T.Text   -- ^ Password
         -> XmppMonad m (Either T.Text ())
saslAuth mechanisms server username password
  | "DIGEST-MD5" `elem` mechanisms = saslDigest server username password
  | otherwise
  = pure
    $  Left
    $  T.pack
    $  "Dont know how to do auth! Available mechanisms are: "
    ++ show mechanisms

saslDigest :: MonadIO m => T.Text -> T.Text -> T.Text -> XmppMonad m (Either T.Text ())
saslDigest server username password = runExceptT $ do
  lift $ xmppSend $ head auth
  ch_text <- (join <$> lift (withNextM getChallenge)) >>= either throwError pure
  resp    <- liftIO $ saslDigestResponse ch_text username server password
  lift $ xmppSend $ head $ response $ T.pack resp
  m <- lift nextM >>= either throwError pure

  unless (null $ tag "failure" m) $ throwError $ "Auth failure: " <> T.pack
    (show m)

  chl_text <- ExceptT . pure $ getChallenge m
  lift (saslDigestRspAuth chl_text) >>= either throwError pure
  lift $ xmppSend $ head sndResponse
  m <- lift nextM >>= either throwError pure
  unless (not $ null $ tag "success" m)
    $  throwError
    $  "Auth failure: "
    <> T.pack (show m)

  where
      auth = [xml|<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="DIGEST-MD5">|]
      response resp = [xml|
        <response xmlns="urn:ietf:params:xml:ns:xmpp-sasl">
          #{resp}
        |]
      sndResponse = [xml|<response xmlns="urn:ietf:params:xml:ns:xmpp-sasl">|]
      getChallenge c =
          case (tag "challenge" /> txt) c of
              [] -> Left "Where is challenge?"
              x  -> Right $ getText_ x

saslDigestResponse :: T.Text -> T.Text -> T.Text -> T.Text -> IO String
saslDigestResponse chl username server password =
  let pairs = getPairs $ B64.decode $ T.unpack chl
      Just qop = lookup "qop" pairs
      Just nonce = lookup "nonce" pairs
      nc = "00000001"
      digest_uri = "xmpp/" ++ T.unpack server
      realm = server 
      in do cnonce <- make_cnonce
            let a1 = semi_sep [ md5raw (MD5.Str (semi_sep $ map T.unpack [username, realm, password])), nonce, cnonce]
                a2 = "AUTHENTICATE:" ++ digest_uri
                t  = semi_sep [ MD5.md5s (MD5.Str a1), nonce, nc, cnonce, qop, MD5.md5s (MD5.Str a2) ]
                response = MD5.md5s (MD5.Str t)
                resp = concat [ "username=", show username
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
  md5raw   = map (chr . read . ("0x"++ ) . take 2) . takeWhile (not.null) . iterate (drop 2) . MD5.md5s
  hexa     = foldr (showHex . ord) ""
  semi_sep = intercalate ":"
  make_cnonce = do g <- newStdGen
                   return $ hexa $ map (chr.(`mod` 256)) $ take 8 $ randoms g

-- | Split aaa=bbb,foo="bar" into [("aaa","bbb"),("foo","bar")]
getPairs :: String -> [(String, String)]
getPairs str = 
  let chunks = map (takeWhile (/=',')) $ takeWhile (not.null) $ iterate (drop 1 . dropWhile (/=',')) str
      (keys, values) = unzip $ map (break (=='=')) chunks
      in zip keys $ map trim values
  where
  -- | Trim leading '=' and surrounding quotes (if any) from value
  trim str = case dropWhile (=='=') str of
                  x@('\"':_) -> read x
                  x          -> x

saslDigestRspAuth :: MonadIO m => T.Text -> XmppMonad m (Either T.Text ())
saslDigestRspAuth chl =
  let pairs = getPairs $ B64.decode $ T.unpack chl
  in  case lookup "rspauth" pairs of
        Just _  -> pure $ Right ()
        Nothing -> pure $ Left "NO rspauth in SASL digest rspauth!"
