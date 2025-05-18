{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Handler where

import Auth.Config
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.String.Conv
import Data.Text (Text, unpack)
import Data.Text.Encoding
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2
import Network.URI
import Servant
import Servant.API.ResponseHeaders
import Servant.Auth.Server
import Servant.Server
import Shared.Models.User
import Shared.ServerResponses (Error)
import Shared.Utils
import System.Environment (getEnv)
import URI.ByteString
import URI.ByteString.QQ
import Web.Cookie

loginHandler :: CookieSettings -> AuthResult User -> Handler String
loginHandler cookieCfg (Authenticated user) = do
  return "Already logged in"
loginHandler cookieCfg _ = do
  state <- liftIO genUrlSafe
  nonce <- liftIO genUrlSafe
  googleOAuth <- liftIO loadGoogleOAuth

  let stateCookie =
        defaultSetCookie
          { setCookieName = "oauth_state",
            setCookieValue = encodeUtf8 state,
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieSameSite = Just sameSiteLax
          }

      nonceCookie =
        defaultSetCookie
          { setCookieName = "oauth_nonce",
            setCookieValue = encodeUtf8 nonce,
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieSameSite = Just sameSiteLax
          }

      authUrl =
        appendQueryParams
          [ ("scope", "openid email profile"),
            ("state", encodeUtf8 state),
            ("nonce", encodeUtf8 nonce)
          ]
          (authorizationUrl googleOAuth)

  case toNetworkURI authUrl of
    Nothing -> throwError err500 {errBody = "Invalid auth URL"}
    Just uri -> do
      let loc = BS.pack $ uriToString id uri ""
      throwError
        err302
          { errHeaders =
              [ ("Set-Cookie", BS.toStrict $ Builder.toLazyByteString $ renderSetCookie stateCookie),
                ("Set-Cookie", BS.toStrict $ Builder.toLazyByteString $ renderSetCookie nonceCookie),
                ("Location", loc)
              ]
          }
      return "Successfully logged in"

logoutHandler ::
  CookieSettings ->
  AuthResult User ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
logoutHandler cs (Authenticated _) = pure $ clearSession cs NoContent
logoutHandler _ _ = throwError err401 {errBody = "Unauthorized"}

testHandler :: AuthResult User -> Handler String
testHandler ar =
  case ar of
    Authenticated user -> return $ name user
    _ -> throwError err401 {errBody = "Unauthorized"}

callbackHandler ::
  CookieSettings ->
  JWTSettings ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
callbackHandler cs jwts mState mCode mCookieHeader = do
  code <- hoistMaybe err400 {errBody = "Missing code"} mCode
  state <- hoistMaybe err400 {errBody = "Missing state param"} mState
  rawCookies <- hoistMaybe err400 {errBody = "Missing cookies"} mCookieHeader
  googleOAuth <- liftIO loadGoogleOAuth

  let cookies = parseCookiesText $ encodeUtf8 rawCookies
      cookieState = lookup "oauth_state" cookies
      cookieNonce = lookup "oauth_nonce" cookies

  cookieStateValue <- hoistMaybe err400 {errBody = "Missing state cookie"} cookieState

  unless (state == cookieStateValue) $ throwError $ err403 {errBody = "State mismatch"}

  manager <- liftIO $ newManager tlsManagerSettings
  tokenResult <- liftIO $ runExceptT $ fetchAccessToken manager googleOAuth (ExchangeToken code)

  accessToken' <- case tokenResult of
    Left _ -> throwError err500 {errBody = "Token exchange failed"}
    Right at -> pure at

  mEmail <- liftIO $ fetchGoogleEmail (accessToken accessToken')
  email <- hoistMaybe err403 {errBody = "Email fetch failed"} mEmail

  let user = User {email = unpack email, age = 20, name = "Hasith"}
  mApplyCookies <- liftIO $ acceptLogin cs jwts user
  applyCookies <- hoistMaybe err500 {errBody = "Could not create JWT"} mApplyCookies
  return $ applyCookies "Successfully logged in"

fetchGoogleEmail :: AccessToken -> IO (Maybe Text)
fetchGoogleEmail token = do
  manager <- newManager tlsManagerSettings
  let url = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
  runExceptT (authGetJSON manager token url) >>= \res ->
    case res of
      Left _ -> return Nothing
      Right obj -> return $ Just $ objEmail obj
  where
    objEmail :: Value -> Text
    objEmail (Object o) = case Aeson.parseMaybe (Aeson..: "email") o of
      Just e -> e
      Nothing -> "unknown"
    objEmail _ = "unknown"
