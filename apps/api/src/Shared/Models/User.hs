{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT, ToJWT)

data User = User
  { user_id :: UUID,
    user_email :: Text,
    user_name :: Text,
    user_created_at :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToJSON User

instance FromJSON User

instance ToJWT User

instance FromJWT User
