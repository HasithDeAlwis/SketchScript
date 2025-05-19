{-# LANGUAGE OverloadedStrings #-}

module Project.DB where

import Data.Pool (Pool, withResource)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, Only(..), query)

import Shared.Models.Project (Project(..))

findProjectsByWorkspaceID :: Pool Connection -> UUID -> IO [Project]
findProjectsByWorkspaceID pool workspaceId =
  withResource pool $ \conn ->
    query
      conn
      "SELECT id AS project_id, workspace_id AS project_workspace_id, name AS project_name, \
      \created_at AS project_created_at, updated_at AS project_updated_at, is_archived AS project_is_archived \
      \FROM projects \
      \WHERE workspace_id = ?"
      (Only workspaceId)
