{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Server (API, server) where

import Auth.Handler
  ( MeHandlerResponse,
    callbackHandler,
    loginHandler,
    logoutHandler,
    meHandler,
  )
import Data.Pool
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple qualified as DBPS
import Servant
import Servant.Auth.Server
import Shared.Models.User

type Protected =
  "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "me" :> Get '[JSON] MeHandlerResponse
    :<|> "login" :> Get '[JSON] String

type API auths =
  (Auth auths User :> Protected)
    :<|> "callback"
      :> QueryParam "state" Text
      :> QueryParam "code" Text
      :> Header "Cookie" Text
      :> Get '[JSON] NoContent

-- :> Get '[JSON] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] User)

server :: Pool DBPS.Connection -> CookieSettings -> JWTSettings -> Server (API auths)
server cons cs jwts =
  protectedHandlers :<|> callbackHandler cons cs jwts
  where
    protectedHandlers :: AuthResult User -> Server Protected
    protectedHandlers authResult =
      logoutHandler cs authResult
        :<|> meHandler cons authResult
        :<|> loginHandler authResult
