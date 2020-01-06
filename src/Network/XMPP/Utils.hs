{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMPP.Utils
-- Copyright   :  (c) pierre, 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  Dmitry Astapov <dastapov@gmail.com>, pierre <k.pierre.k@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Various XMPP\/XML utilities 
--
-----------------------------------------------------------------------------

module Network.XMPP.Utils
  ( strAttr
  , getVals
  , isVal
  , getText
  , getText_
  , txtpat
  , xtractp
  , matchPatterns
  , mread
  , mattr
  , mattr'
  , debug
  , debugIO
  , literal -- from HaXML
  ) where

import           Text.XML.HaXml                 hiding (tag)
import           Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty           as P
import           Text.XML.HaXml.Xtract.Parse     (xtract)
import           Text.PrettyPrint.HughesPJ       (hcat)
import           Data.Text                       (Text, pack, unpack)
import           Control.Monad.IO.Class          (liftIO)

import Network.XMPP.Types

-- | Conversion from\/to HaXML's Content and CFilter 
--toContent :: CFilter Posn -> Content Posn
--toContent f =
--    head $ f (CElem noelem noPos)


strAttr :: a -> String -> (a, CFilter i)
strAttr s d = (s, literal d)

-- | Returns strings extracted by xtract query 
getVals :: Text -> [Content Posn] -> [Text]
getVals q = map (getText_ . xtract id (unpack q))

-- | Queries xml for specific value
-- @isVal str = any (== str) . getVals@
isVal :: Text -> Text -> [Content Posn] -> Bool
isVal str cont = any (== str) . getVals cont

-- 
getText :: Content i -> Text
getText cs@CString{}  = pack . render . P.content $ cs
getText cs@CRef{}     = pack . render . P.content $ cs
getText x               = error $ "Attempt to extract text from content that is not a string: " ++ render ( P.content x )

getText_ :: [Content i] -> Text
getText_ = pack . render . hcat . map P.content

-- | Extract text from `Content Posn' with supplied pattern
txtpat :: Text      -- ^ xtract-like pattern to match
    -> Content Posn -- ^ message being processed
    -> Text         -- ^ result of extraction
txtpat p m = getText_ $ xtract id (unpack p) m

xtractp :: (Text -> Text) -> Text -> Content i -> Bool
xtractp f p m = not . null $ xtract (unpack . f . pack) (unpack p) m

matchPatterns :: Content i -> [Text] -> Bool
matchPatterns m = all $ flip (xtractp id) m

mread :: Read a => String -> Maybe a
mread "" = Nothing
mread a = Just $ read a

mattr :: (Show a) => b -> Maybe a -> [(b, CFilter i)]
mattr s (Just a) = [ strAttr s (show a) ]
mattr _ Nothing = []

mattr' :: a -> Maybe String -> [(a, CFilter i)]
mattr' s (Just a) = [ strAttr s a ]
mattr' _ Nothing = []

debug :: String -> XmppMonad ()
debugIO :: String -> IO ()
#ifdef DEBUG
debug = liftIO . putStrLn
debugIO = putStrLn
#else
debug _ = return ()
debugIO _ = return ()
#endif
