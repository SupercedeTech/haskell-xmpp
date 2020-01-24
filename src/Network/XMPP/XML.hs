{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.XML
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
  , literal -- from HaXML
  , noelem
  , lookupAttr
  , FromXML(..)
  , ToXML(..)
  ) where

import           Text.XML                       (Node)
import           Text.XML.HaXml                 hiding (tag)
import           Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Pretty           as P
import           Text.XML.HaXml.Xtract.Parse     (xtract)
import           Text.PrettyPrint.HughesPJ       (hcat)
import           Data.Text                       (Text, pack, unpack)
import           Text.Read
import           Control.Applicative             ((<|>))

class FromXML a where
  decodeXml :: Content Posn -> Maybe a

class ToXML a where
  encodeXml :: a -> [Node]

instance FromXML () where
  decodeXml _ = Just ()

instance (FromXML a, FromXML b) => FromXML (Either a b) where
  decodeXml m = (Left <$> decodeXml m) <|> (Right <$> decodeXml m)

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
getText cs@CString{} = pack . render . P.content $ cs
getText cs@CRef{}    = pack . render . P.content $ cs
getText x =
  error
    $  "Attempt to extract text from content that is not a string: "
    ++ render (P.content x)

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

mread :: Read a => Text -> Maybe a
mread "" = Nothing
mread a = readMaybe $ unpack a

mattr :: (Show a) => b -> Maybe a -> [(b, CFilter i)]
mattr s (Just a) = [ strAttr s (show a) ]
mattr _ Nothing = []

mattr' :: a -> Maybe String -> [(a, CFilter i)]
mattr' s (Just a) = [ strAttr s a ]
mattr' _ Nothing = []

noelem :: Content Posn
noelem = CElem (Elem (N "root") [] []) noPos

lookupAttr :: String -> [Attribute] -> Maybe String
lookupAttr k lst = do
  x <- lookup (N k) lst
  case x of
    AttValue [Left str] -> Just str
    AttValue _          -> Nothing
