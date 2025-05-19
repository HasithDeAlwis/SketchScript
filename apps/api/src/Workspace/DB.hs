{-# LANGUAGE OverloadedStrings #-}

module Workspace.DB where

import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Shared.Models.Project
import Shared.Models.Role
import Shared.Models.Workspace

createWorkspace ::
  Pool Connection ->
  -- | Optional workspace name
  Maybe Text ->
  -- | Optional workspace type
  Maybe Text ->
  IO (Workspace, Role, Project)
createWorkspace pool mName mType =
  let name = fromMaybe "Untitled" mName
      workspaceType = fromMaybe "personal" mType
   in withResource pool $ \conn -> do
        -- Insert Workspace
        [workspace] <-
          query
            conn
            "INSERT INTO workspaces (id, name, type) \
            \VALUES (gen_random_uuid(), ?, ?) \
            \RETURNING id AS workspace_id, name AS workspace_name, type AS workspace_type, created_at AS workspace_created_at"
            (name, workspaceType)

        -- Insert Admin Role
        [role] <-
          query
            conn
            "INSERT INTO roles (id, workspace_id, name, can_create_project, can_edit_project, can_delete_project, can_export, can_manage_members) \
            \VALUES (gen_random_uuid(), ?, 'admin', TRUE, TRUE, TRUE, TRUE, TRUE) \
            \RETURNING id AS role_id, workspace_id AS role_workspace_id, name AS role_name, \
            \can_create_project, can_edit_project, can_delete_project, can_export, can_manage_members"
            (Only $ workspace_id workspace)

        -- Insert Default Project
        [project] <-
          query
            conn
            "INSERT INTO projects (id, workspace_id, name) \
            \VALUES (gen_random_uuid(), ?, 'Untitled Project') \
            \RETURNING id AS project_id, workspace_id AS project_workspace_id, name AS project_name, \
            \created_at AS project_created_at, updated_at AS project_updated_at, is_archived AS project_is_archived"
            (Only $ workspace_id workspace)

        return (workspace, role, project)

joinWorkspace :: Pool Connection -> UUID -> UUID -> UUID -> IO ()
joinWorkspace pool userId workspaceId roleId =
  withResource pool $ \conn -> do
    _ <-
      execute
        conn
        "INSERT INTO workspace_memberships (id, user_id, workspace_id, role_id) \
        \VALUES (gen_random_uuid(), ?, ?, ?)"
        (userId, workspaceId, roleId)
    return ()

getUserWorkspaces :: Pool Connection -> UUID -> IO [Workspace]
getUserWorkspaces pool userID = withResource pool $ \conn ->
  query
    conn
    "SELECT w.id AS workspace_id, w.name AS workspace_name, w.type AS workspace_type, w.created_at AS workspace_created_at \
    \FROM workspaces w \
    \INNER JOIN workspace_memberships wm ON wm.workspace_id = w.id \
    \WHERE wm.user_id = ?"
    (Only userID)
