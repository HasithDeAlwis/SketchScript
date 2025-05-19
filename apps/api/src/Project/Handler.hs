{-# LANGUAGE OverloadedStrings #-}

module Project.Handler where

import Data.Pool (Pool)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import Project.DB (findProjectsByWorkspaceID)
import Shared.Models.Project (Project)

getProjectsByWorkspaceIO :: Pool Connection -> Text -> IO (Either Text [Project])
getProjectsByWorkspaceIO pool widText =
  case UUID.fromText widText of
    Nothing -> pure $ Left "Invalid workspace ID"
    Just wid -> do
      projects <- findProjectsByWorkspaceID pool wid
      pure $ Right projects
