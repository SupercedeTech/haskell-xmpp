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
  , mread
  , mattr
  , mattr'
  , debug
  , debugIO
  , literal -- from HaXML
  ) where

import Text.XML.HaXml hiding (tag)
import Text.XML.HaXml.Posn     
import qualified Text.XML.HaXml.Pretty as P
import Text.PrettyPrint.HughesPJ  (hcat)
import Text.XML.HaXml.Xtract.Parse (xtract)
    
import Network.XMPP.Types

-- | Conversion from\/to HaXML's Content and CFilter 
--toContent :: CFilter Posn -> Content Posn
--toContent f =
--    head $ f (CElem noelem noPos)


strAttr :: a -> String -> (a, CFilter i)
strAttr s d = (s, literal d)

-- | Returns strings extracted by xtract query 
getVals :: String ->
          [Content Posn] ->
          [String]
getVals q ext = map (\x -> getText_ $ xtract id q x) ext

-- | Queries xml for specific value
-- @isVal str = any (== str) . getVals@
isVal :: String -> String -> [Content Posn] -> Bool
isVal str cont = any (== str) . (getVals cont)

-- 
getText :: Content i -> String
getText cs@(CString{})  = render . P.content $ cs
getText cs@(CRef{})     = render . P.content $ cs
getText x               = error $ "Attempt to extract text from content that is not a string: " ++ render ( P.content x )

getText_ :: [Content i] -> String
getText_ = render . hcat . map P.content
           
mread :: (Read a) => [Char] -> Maybe a
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
