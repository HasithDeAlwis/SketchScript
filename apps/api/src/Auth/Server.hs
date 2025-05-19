{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Server (API, server) where

import Auth.Handler
  ( callbackHandler,
    loginHandler,
    logoutHandler,
    testHandler,
  )
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as DBPS
import Data.UUID (UUID)
import Servant
import Servant.Auth.Server
import Shared.Models.User

type Protected =
  "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "test" :> Get '[JSON] User
    :<|> "login" :> Get '[JSON] String

type API auths =
  (Auth auths User :> Protected)
    :<|> "callback"
      :> QueryParam "state" Text
      :> QueryParam "code" Text
      :> Header "Cookie" Text
      :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UUID)

server :: Pool DBPS.Connection -> CookieSettings -> JWTSettings -> Server (API auths)
server cons cs jwts =
  protectedHandlers :<|> callbackHandler cons cs jwts
  where
    protectedHandlers :: AuthResult User -> Server Protected
    protectedHandlers authResult =
      logoutHandler cs authResult
        :<|> testHandler authResult
        :<|> loginHandler authResult
