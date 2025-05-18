{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Auth.Config where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Text qualified as T
import Network.OAuth.OAuth2 (OAuth2 (..))
import System.Environment (getEnv)
import URI.ByteString.QQ (uri)

loadGoogleOAuth :: IO OAuth2
loadGoogleOAuth = do
  _ <- loadFile defaultConfig
  clientID <- getEnv "GOOGLE_CLIENT_ID"
  clientSecret <- getEnv "GOOGLE_CLIENT_SECRET"
  return $
    OAuth2
      { oauth2ClientId = T.pack clientID,
        oauth2ClientSecret = T.pack clientSecret,
        oauth2AuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|],
        oauth2TokenEndpoint = [uri|https://oauth2.googleapis.com/token|],
        oauth2RedirectUri = [uri|http://localhost:8080/callback|]
      }
