{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.File where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data File = File
  { file_id :: UUID,
    file_project_id :: UUID,
    file_parent_folder_id :: Maybe UUID,
    file_name :: Text,
    file_type :: Text,
    file_content :: Maybe Text,
    file_created_at :: UTCTime,
    file_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance ToJSON File
instance FromJSON File
instance FromRow File
