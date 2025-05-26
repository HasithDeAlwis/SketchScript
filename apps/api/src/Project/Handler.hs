{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Project.Handler where

import Data.Aeson (FromJSON)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Project.DB (findProjectByID, findProjectsByWorkspaceID, updateProjectName)
import Shared.Models.Project (Project)

newtype ProjectNameUpdate = ProjectNameUpdate
  { name :: Text
  }
  deriving (Show, Generic)

instance FromJSON ProjectNameUpdate

getProjectsByWorkspaceIO :: Pool Connection -> Text -> IO (Either Text [Project])
getProjectsByWorkspaceIO pool widText =
  case UUID.fromText widText of
    Nothing -> pure $ Left "Invalid workspace ID"
    Just wid -> do
      projects <- findProjectsByWorkspaceID pool wid
      pure $ Right projects

updateProjectNameIO :: Pool Connection -> Text -> Text -> IO (Either Text Project)
updateProjectNameIO pool projectIdText newName =
  case UUID.fromText projectIdText of
    Nothing -> pure $ Left "Invalid project ID"
    Just pid -> do
      updateProjectName pool pid newName
      projects <- findProjectByID pool pid
      case projects of
        [project] -> pure $ Right project
        _ -> pure $ Left "Project not found"
