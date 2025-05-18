{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Server (app) where

import Auth.Server qualified as Auth (API, server)
import Configuration.Dotenv (defaultConfig, loadFile)
import Servant
import Servant.Auth.Server
import System.Environment (getEnv)
import User.Api qualified as User (API)
import User.Server qualified as User

type API auths = User.API :<|> Auth.API auths

coreServer :: CookieSettings -> JWTSettings -> Server (API auths)
coreServer cs jwts =
  User.server :<|> Auth.server cs jwts

app :: IO Application
app = do
  _ <- loadFile defaultConfig
  env <- getEnv "ENV"
  jwtKey <- generateKey

  let cookieCfg =
        if env == "PROD"
          then
            defaultCookieSettings
              { cookieIsSecure = Secure,
                cookieSameSite = SameSiteLax,
                cookieXsrfSetting = Just def
              }
          else defaultCookieSettings {cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing}
      jwtSettings = defaultJWTSettings jwtKey
      context = cookieCfg :. jwtSettings :. EmptyContext
      cookieAPI = Proxy :: Proxy (API '[Cookie])
  return $ serveWithContext cookieAPI context (coreServer cookieCfg jwtSettings)
