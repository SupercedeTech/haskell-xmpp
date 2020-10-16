{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ejabberd api support
module Network.XMPP.Ejabberd
  ( EjabberdHost(..)
  , EUser(..)
  , VHost(..)
  , EResult(..)
  , RegisterUserReq(..)
  , localEjabberdHost
  , getRegisteredUsers
  , registerNewUser
  ) where

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client (RequestBody(..), Response)
import Network.HTTP.Simple
       (getResponseBody, httpLBS, parseRequest_, setRequestBody)
import Control.Exception
import Data.Text(Text)
import qualified Data.Text as Text
import Text.Printf
import Data.Char (isLower)
import Control.Applicative

data EUser =
  EUser
  { euName :: Text
  , euPassword :: Text
  } deriving (Eq, Show, Generic)

newtype VHost =
  VHost { vhHost :: Text }
  deriving (Eq, Show, Generic)

data EResult a
  = ESuccess a
  | EError
    { eStatus  :: Text
    , eCode    :: Int
    , eMessage :: Text
    }
  deriving (Eq, Show)

data RegisterUserReq = RegisterUserReq
  { rurUser :: Text
  , rurPassword :: Text
  , rurHost :: Text
  } deriving (Eq, Show, Generic)

instance J.FromJSON a => J.FromJSON (EResult a) where
  parseJSON raw =
    let failed = flip (J.withObject "EjabberdResponse") raw $ \o -> do
          status <- o J..: "status"
          code   <- o J..: "code"
          msg    <- o J..: "message"
          pure $ EError status code msg
        success = ESuccess <$> J.parseJSON raw
    in  success <|> failed

instance J.ToJSON VHost where
  toJSON = J.genericToJSON snakeLabel

instance J.ToJSON RegisterUserReq where
  toJSON = J.genericToJSON snakeLabel

newtype EjabberdHost = EjabberdHost String

snakeConstructor :: J.Options
snakeConstructor = J.defaultOptions { J.constructorTagModifier = J.camelTo2 '_' }

snakeLabel :: J.Options
snakeLabel = snakeConstructor { J.fieldLabelModifier = J.camelTo2 '_' . dropWhile isLower }

localEjabberdHost :: EjabberdHost
localEjabberdHost = EjabberdHost "http://localhost:5443"

toPath :: EjabberdHost -> String -> String
toPath (EjabberdHost d) = printf "POST %s/%s" d

-- | https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#registered-users
--
--  @since 2.0.0
getRegisteredUsers :: EjabberdHost -> VHost -> IO (EResult [Text])
getRegisteredUsers ejabberd vhost = do -- TODO: monad reader with api host and manager
  let body = RequestBodyLBS $ J.encode vhost
      path = toPath ejabberd "api/registered_users"
      req  = setRequestBody body $ parseRequest_ path

  resp :: Either SomeException (Response BSL.ByteString) <-
    try $ httpLBS req
  let eiResult = returnable . J.eitherDecode . getResponseBody <$> resp
  pure $ either (EError "exception" (-1) . Text.pack . show) id eiResult
  where returnable = either (EError "error" (-1) . Text.pack) id

-- | https://docs.ejabberd.im/developer/ejabberd-api/admin-api/#register
--
--  @since 2.0.0
registerNewUser :: EjabberdHost -> EUser -> VHost -> IO (EResult Text)
registerNewUser ejabberd newUser h = do
  let body = RegisterUserReq (euName newUser) (euPassword newUser) $ vhHost h
      encodedBody = RequestBodyLBS $ J.encode body
      path        = toPath ejabberd "api/register"
      req         = setRequestBody encodedBody $ parseRequest_ path

  resp :: Either SomeException (Response BSL.ByteString) <-
    try $ httpLBS req
  let eiResult = returnable . J.eitherDecode . getResponseBody <$> resp
  pure $ either (EError "exception" (-1) . Text.pack . show) id eiResult
  where returnable = either (EError "error" (-1) . Text.pack) id
