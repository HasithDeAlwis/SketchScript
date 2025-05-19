{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.Project where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Project = Project
  { project_id :: UUID,
    project_workspace_id :: UUID,
    project_name :: Text,
    project_created_at :: UTCTime,
    project_updated_at :: UTCTime,
    project_is_archived :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Project

instance FromJSON Project

instance FromRow Project
