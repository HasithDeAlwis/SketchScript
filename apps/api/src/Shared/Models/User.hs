{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT, ToJWT)



data User = User
  { name :: String,
    age :: Int,
    email :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

instance ToJWT User

instance FromJWT User
