{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Handler where

import Auth.Config
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, Value (..))
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust, fromMaybe)
import Data.Pool
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Data.UUID (UUID, fromString)
import Database.PostgreSQL.Simple qualified as DBPS
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2
import Network.URI
import Servant
import Servant.Auth.Server
import Shared.Models.Role (Role (..))
import Shared.Models.User (User (..))
import Shared.Models.Workspace (Workspace (..))
import Shared.Utils
import URI.ByteString ()
import URI.ByteString.QQ
import User.DB (createUser, getUserByEmail)
import Web.Cookie
import Workspace.DB (createWorkspace, getUserWorkspaces, joinWorkspace)

uuid :: String -> UUID
uuid = fromJust . fromString

-- Hardcoded UTCTime helper (ISO 8601 string)
time :: String -> UTCTime
time = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

loginHandler :: AuthResult User -> Handler String
loginHandler (Authenticated _user) = do
  return "Already logged in"
loginHandler _ = do
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
    Just newUri -> do
      let loc = BS.pack $ uriToString id newUri ""
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

data MeHandlerResponse = MeHandlerResponse
  { user :: User,
    workspaces :: [Workspace]
  }
  deriving (Show, Generic)

instance ToJSON MeHandlerResponse

meHandler :: Pool DBPS.Connection -> AuthResult User -> Handler MeHandlerResponse
meHandler cons (Authenticated curUser) = do
  userWorkspaces <- liftIO $ getUserWorkspaces cons $ user_id curUser
  return $ MeHandlerResponse curUser userWorkspaces
meHandler _ _ = throwError err401 {errBody = "User is not authorized"}

callbackHandler ::
  Pool DBPS.Connection ->
  CookieSettings ->
  JWTSettings ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Handler NoContent
-- Handler (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] User)
callbackHandler conn cs jwts mState mCode mCookieHeader = do
  code <- hoistMaybe err400 {errBody = "Missing code"} mCode
  state <- hoistMaybe err400 {errBody = "Missing state param"} mState
  rawCookies <- hoistMaybe err400 {errBody = "Missing cookies"} mCookieHeader
  googleOAuth <- liftIO loadGoogleOAuth

  let cookies = parseCookiesText $ encodeUtf8 rawCookies
      cookieState = lookup "oauth_state" cookies
      _cookieNonce = lookup "oauth_nonce" cookies -- unused for now, will be implemented later
  cookieStateValue <- hoistMaybe err400 {errBody = "Missing state cookie"} cookieState

  unless (state == cookieStateValue) $ throwError $ err403 {errBody = "State mismatch"}

  manager <- liftIO $ newManager tlsManagerSettings
  tokenResult <- liftIO $ runExceptT $ fetchAccessToken manager googleOAuth (ExchangeToken code)

  accessToken' <- case tokenResult of
    Left _ -> throwError err500 {errBody = "Token exchange failed"}
    Right at -> pure at

  mGoogleUser <- liftIO $ fetchGoogleUser (accessToken accessToken')
  GoogleUser {guEmail = mEmail, guName = mName} <-
    hoistMaybe err403 {errBody = "Google user object missing"} mGoogleUser

  userEmail <- hoistMaybe err403 {errBody = "Email missing from Google response"} mEmail
  userName <- hoistMaybe err403 {errBody = "Name missing from Google response"} mName

  possibleUser <- liftIO $ getUserByEmail conn userEmail

  (curUser, _workspaceID : _rest) <- case possibleUser of
    [] -> do
      curUser <- liftIO $ createUser conn userName userEmail
      (workspace, role, _project) <- liftIO $ createWorkspace conn (Just "First Project") (Just "personal")
      () <- liftIO $ joinWorkspace conn (user_id curUser) (workspace_id workspace) (role_id role)
      pure (curUser, [workspace_id workspace])
    [curUser] -> do
      workspace <- liftIO $ getUserWorkspaces conn (user_id curUser)
      pure (curUser, map workspace_id workspace)
    _ -> throwError err403 {errBody = "Duplicate emails found"}

  mApplyCookies <- liftIO $ acceptLogin cs jwts curUser
  applyCookies <- hoistMaybe err500 {errBody = "Could not create JWT"} mApplyCookies
  let responseWithCookies = applyCookies NoContent
      allHeaders = getHeaders responseWithCookies

  -- Extract only Set-Cookie headers
  let setCookieHeaders =
        [ (hName, hVal)
          | (hName, hVal) <- allHeaders,
            hName == "Set-Cookie"
        ]

  let redirectLocation = ("Location", "http://localhost:4200/login")

  throwError
    err302
      { errHeaders = redirectLocation : setCookieHeaders
      }
  return NoContent

data GoogleUser = GoogleUser {guEmail :: Maybe Text, guName :: Maybe Text}

fetchGoogleUser :: AccessToken -> IO (Maybe GoogleUser)
fetchGoogleUser token = do
  manager <- newManager tlsManagerSettings
  let url = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
  runExceptT (authGetJSON manager token url) >>= \case
    Left _ -> return Nothing
    Right (Object o) -> do
      let parsedEmail = fromMaybe "unknown" $ Aeson.parseMaybe (Aeson..: "email") o
          parsedName = fromMaybe "unknown" $ Aeson.parseMaybe (Aeson..: "name") o
      return $
        Just
          GoogleUser
            { guEmail = if parsedEmail == "unknown" then Nothing else Just parsedEmail,
              guName = if parsedName == "unknown" then Nothing else Just parsedName
            }
    Right _ -> return Nothing
