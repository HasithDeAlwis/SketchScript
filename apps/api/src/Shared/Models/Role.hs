{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.Role where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Role = Role
  { role_id :: UUID,
    role_workspace_id :: UUID,
    role_name :: Text,
    role_can_create_project :: Bool,
    role_can_edit_project :: Bool,
    role_can_delete_project :: Bool,
    role_can_export :: Bool,
    role_can_manage_members :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Role

instance FromJSON Role

instance FromRow Role
