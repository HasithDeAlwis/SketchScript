{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module App.Server (app) where

import Auth.Server qualified as Auth (API, server)
import Configuration.Dotenv (defaultConfig, loadFile)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Pool
import Database.PostgreSQL.Simple qualified as DBPS
import Network.Wai.Middleware.Cors
import Servant
import Servant.Auth.Server
import System.Environment (getEnv)
import User.Api qualified as User (API)
import User.Server qualified as User

type DBConnectionString = ByteString

type API auths = User.API :<|> Auth.API auths

initConnectionPool :: DBConnectionString -> IO (Pool DBPS.Connection)
initConnectionPool connStr =
  createPool
    (DBPS.connectPostgreSQL connStr)
    DBPS.close
    2 -- stripes
    60 -- unused connections are kept open for a minute
    10 -- max. 10 connections open per stripe

coreServer :: Pool DBPS.Connection -> CookieSettings -> JWTSettings -> Server (API auths)
coreServer cons cs jwts =
  User.server :<|> Auth.server cons cs jwts

corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:4200"], True),
      corsRequestHeaders = ["Authorization", "Content-Type"],
      corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    }

app :: IO Application
app = do
  _ <- loadFile defaultConfig
  env <- getEnv "ENV"
  connStr <- getEnv "DATABASE_URL"
  pool <- initConnectionPool $ pack connStr
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
  return $ cors (const $ Just corsPolicy) $ serveWithContext cookieAPI context (coreServer pool cookieCfg jwtSettings)
