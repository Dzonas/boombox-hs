{-# LANGUAGE OverloadedStrings #-}

module Spotihy where

import Data.Aeson
import Data.Base64.Types (extractBase64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Base64
import Network.HTTP.Simple

authHost :: Text
authHost = "accounts.spotify.com"

data AuthConfig = AuthConfig {clientId :: Text, clientSecret :: Text} deriving (Show)

data ClientCredentials = ClientCredentials {accessToken :: Text, tokenType :: Text, expiresIn :: Word} deriving (Show)

instance FromJSON ClientCredentials where
  parseJSON = withObject "ClientCredentials" $ \v ->
    ClientCredentials
      <$> v .: "access_token"
      <*> v .: "token_type"
      <*> v .: "expires_in"

getClientCredentials :: AuthConfig -> IO ClientCredentials
getClientCredentials config = do
  let request =
        addRequestHeader "Authorization" authorizationHeader
          . setRequestBodyURLEncoded [("grant_type", "client_credentials")]
          . setRequestPath "/api/token"
          . setRequestHost (encodeUtf8 authHost)
          . setRequestSecure True
          . setRequestPort 443
          $ defaultRequest
  response <- httpJSON request
  return $ getResponseBody response
  where
    base64EncodedToken = extractBase64 . encodeBase64 $ clientId config <> ":" <> clientSecret config
    authorizationHeader = encodeUtf8 ("Basic " <> base64EncodedToken)
