{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Server (API, server) where

import Auth.Handler
  ( callbackHandler,
    loginHandler,
    logoutHandler,
    testHandler,
  )
import Data.Text (Text)
import Servant
import Servant.Auth.Server
import Shared.Models.User

type Protected =
  "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "test" :> Get '[JSON] String
    :<|> "login" :> Get '[JSON] String

type API auths =
  (Auth auths User :> Protected)
    :<|> "callback"
      :> QueryParam "state" Text
      :> QueryParam "code" Text
      :> Header "Cookie" Text
      :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts =
  protectedHandlers :<|> callbackHandler cs jwts
  where
    protectedHandlers :: AuthResult User -> Server Protected
    protectedHandlers authResult =
      logoutHandler cs authResult
        :<|> testHandler authResult
        :<|> loginHandler cs authResult
