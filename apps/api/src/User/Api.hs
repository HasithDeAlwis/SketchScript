{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module User.Api where

import Servant (Get, JSON, (:<|>), (:>))
import Shared.Models.User (User)

type API =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "issac" :> Get '[JSON] User
