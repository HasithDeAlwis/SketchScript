{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.Workspace where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Workspace = Workspace
  { workspace_id :: UUID,
    workspace_name :: Text,
    workspace_type_ :: Text,
    workspace_created_at :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON Workspace

instance FromJSON Workspace

instance FromRow Workspace

data WorkspaceMembership = WorkspaceMembership
  { workspace_membership_id :: UUID,
    workspace_membership_user_id :: UUID,
    workspace_membership_workspace_id :: UUID,
    workspace_membership_role_id :: UUID,
    workspace_membership_joined_at :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON WorkspaceMembership

instance FromJSON WorkspaceMembership

instance FromRow WorkspaceMembership
