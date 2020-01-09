{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}

module Network.XMPP.XEP.Form where

import           Text.Hamlet.XML             (xml)
import           Text.XML.HaXml.Xtract.Parse (xtract)

import           Data.Maybe
import           Data.List                   (find)
import qualified Data.Text                   as T

import           Network.XMPP.XML

-- Specification:
-- https://xmpp.org/extensions/xep-0004.html#table-2
--

-- https://xmpp.org/extensions/xep-0004.html#table-2
instance FromXML XmppField where
  decodeXml m =
    let _label   = txtpat "/field/@label" m
        typ      = txtpat "/field/@type" m
        variable = txtpat "/field/@var" m
    in  case typ of
          "boolean"     -> BooleanField variable <$> boolVal
          "text-single" -> Just $ SingleTextField variable txtSingleVal
          "list-single" ->
            Just $ ListSingleField variable listOptions txtSingleVal
          "list-multi" -> Just $ ListMultiField variable listOptions listValues
          "hidden"     -> Just $ HiddenField variable txtSingleVal
          _            -> Nothing
    where
      listValues   = txtpat "/value/-" <$> xtract id "/field/value/" m
      listOptions  = txtpat "/value/-" <$> xtract id "/field/option/value" m
      txtSingleVal = txtpat "/field/value/-" m
      boolVal      = case txtpat "/field/value/-" m of
        "0" -> Just False
        "1" -> Just True
        _   -> Nothing


newtype XmppForm = XmppForm [XmppField] deriving (Eq, Show)

type FieldName = T.Text

data XmppField =
    SingleTextField
    { xfName  :: FieldName
    , stfValue :: T.Text
    }
  | ListSingleField
    { xfName    :: FieldName
    , lsfOptions :: [T.Text]
    , lsfValue   :: T.Text
    }
  | BooleanField
    { xfName  :: FieldName
    , bfValue :: Bool
    }
  | ListMultiField
    { xfName    ::FieldName
    , lmfOptions :: [T.Text]
    , lmfValue   :: [T.Text]
    }
  | HiddenField { xfName :: T.Text, hfValue :: T.Text }
  deriving (Eq, Show)

updateFormField :: FieldName -> (XmppField -> XmppField) -> XmppForm -> XmppForm
updateFormField fname update (XmppForm fields) =
  let mField = update <$> find ((== fname) . xfName) fields
      nextFields =
          (<> maybeToList mField) . filter ((/= fname) . xfName) $ fields
  in  XmppForm nextFields

setBoolValue :: Bool -> XmppField -> XmppField
setBoolValue val (BooleanField name _) = BooleanField name val
setBoolValue _ field = field

instance FromXML XmppForm where
  decodeXml = Just . XmppForm . mapMaybe decodeXml . xtract id "/x/field"

instance ToXML XmppForm where
  encodeXml (XmppForm fields) =
    [xml|
      <x xmlns="jabber:x:data" type="submit">
        $forall field <- fields
          $case field
            $of HiddenField name value
              <field var=#{name}>
                <value>#{value}

            $of SingleTextField name value
              <field var=#{name}>
                <value>#{value}

            $of BooleanField name value
              <field var=#{name}>
                <value>
                  $if value
                    1
                  $else
                    0

            $of ListSingleField name _opts value
              <field var=#{name}>
                <value>#{value}

            $of ListMultiField name _opts values
              <field var=#{name}>
                $forall value <- values
                  <value>#{value}
    |]