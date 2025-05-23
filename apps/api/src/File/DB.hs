{-# LANGUAGE OverloadedStrings #-}

module File.DB where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Shared.Models.File

createFile :: Pool Connection -> UUID -> Maybe UUID -> Maybe Text -> Text -> IO File
createFile pool projectId mParentId mName fileType = withResource pool $ \conn -> do
  [file] <-
    query
      conn
      "INSERT INTO files (id, project_id, parent_folder_id, name, type) \
      \VALUES (gen_random_uuid(), ?, ?, ?, ?) \
      \RETURNING id AS file_id, project_id AS file_project_id, parent_folder_id AS file_parent_folder_id, \
      \ name AS file_name, type AS file_type, content AS file_content, \
      \ created_at AS file_created_at, updated_at AS file_updated_at"
      (projectId, mParentId, fromMaybe "Untitled File" mName, fileType)
  return file

updateFile :: Pool Connection -> UUID -> UUID -> Maybe UUID -> Maybe Text -> IO File
updateFile pool fileId projectId mParentId mName = withResource pool $ \conn -> do
  [file] <-
    query
      conn
      "UPDATE files \
      \SET parent_folder_id = ?, name = ?, updated_at = NOW() \
      \WHERE id = ? AND project_id = ? \
      \RETURNING id AS file_id, project_id AS file_project_id, parent_folder_id AS file_parent_folder_id, \
      \ name AS file_name, type AS file_type, content AS file_content, \
      \ created_at AS file_created_at, updated_at AS file_updated_at"
      (mParentId, fromMaybe "Untitled File" mName, fileId, projectId)
  return file

deleteFile :: Pool Connection -> UUID -> IO ()
deleteFile pool fileId = withResource pool $ \conn ->
  void $ execute conn "DELETE FROM files WHERE id = ? OR parent_folder_id = ?" (fileId, fileId)

getFilesByProjectID :: Pool Connection -> UUID -> IO [File]
getFilesByProjectID pool projectId =
  withResource pool $ \conn ->
    query
      conn
      "SELECT id AS file_id, project_id AS file_project_id, parent_folder_id AS file_parent_folder_id, \
      \name AS file_name, type AS file_type, content AS file_content, \
      \created_at AS file_created_at, updated_at AS file_updated_at \
      \FROM files WHERE project_id = ?"
      (Only projectId)
