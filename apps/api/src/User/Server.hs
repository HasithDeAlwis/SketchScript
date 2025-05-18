{-# LANGUAGE DataKinds #-}

module User.Server (server) where

import Servant (Server, (:<|>)(..))
import User.Api (API)
import User.Handler (albert, isaac, users)

server :: Server API
server =
  return users
    :<|> return albert
    :<|> return isaac
