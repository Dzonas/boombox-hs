{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotihy where

import Data.Aeson
import Data.Base64.Types (extractBase64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Base64
import GHC.Generics
import Network.HTTP.Simple

authHost :: Text
authHost = "accounts.spotify.com"

apiHost :: Text
apiHost = "api.spotify.com"

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

newtype Track = Track {trackId :: Text} deriving (Show)

instance FromJSON Track where
  parseJSON = withObject "track" $ \o -> do
    trackItems <- o .: "tracks"
    items <- trackItems .: "items"
    case items of
      [] -> fail "No track found"
      (x : _) -> Track <$> (x .: "id")

searchTrackName :: ClientCredentials -> Text -> IO Track
searchTrackName (ClientCredentials accessToken _ _) trackName = do
  let request =
        setRequestQueryString [("q", Just $ encodeUtf8 trackName), ("type", Just "track")]
          . addRequestHeader "Authorization" authorizationHeader
          . setRequestPath "/v1/search"
          . setRequestHost (encodeUtf8 apiHost)
          . setRequestSecure True
          . setRequestPort 443
          $ defaultRequest
  response <- httpJSON request
  return $ getResponseBody response
  where
    authorizationHeader = encodeUtf8 $ "Bearer " <> accessToken

data PlayTrackRequestBody = PlayTrackRequestBody {uris :: [Text]}
  deriving
    ( Show,
      Generic
    )

instance ToJSON PlayTrackRequestBody

playTrack :: ClientCredentials -> Track -> IO Value
playTrack (ClientCredentials accessToken _ _) (Track trackId) = do
  let request =
        setRequestBodyJSON (PlayTrackRequestBody ["spotify:track:" <> trackId])
          . addRequestHeader "Authorization" authorizationHeader
          . setRequestPath "/v1/me/player/play"
          . setRequestHost (encodeUtf8 apiHost)
          . setRequestSecure True
          . setRequestPort 443
          . setRequestMethod "PUT"
          $ defaultRequest
  response <- httpJSON request
  return $ getResponseBody response
  where
    authorizationHeader = encodeUtf8 $ "Bearer " <> accessToken
