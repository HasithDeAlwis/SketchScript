{-# LANGUAGE OverloadedStrings #-}

module File.Handler where

import Data.Pool (Pool)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import File.DB
import File.Types
import Shared.Models.File

createFileHandler :: Pool Connection -> CreateFileReq -> IO File
createFileHandler pool (CreateFileReq pid parent name fileType) = createFile pool pid parent name fileType

updateFileHandler :: Pool Connection -> UpdateFileReq -> IO File
updateFileHandler pool (UpdateFileReq fileId pid parent name) = updateFile pool fileId pid parent name

deleteFileHandler :: Pool Connection -> UUID -> IO ()
deleteFileHandler = deleteFile

toFileResponse :: File -> FileResponse
toFileResponse f =
  FileResponse
    { resFileID = file_id f,
      resFileName = file_name f,
      resFileParent = file_parent_folder_id f
    }

getFilesByProjectIO :: Pool Connection -> Text -> IO (Either Text [File])
getFilesByProjectIO pool pidText =
  case UUID.fromText pidText of
    Nothing -> pure $ Left "Invalid project ID"
    Just pid -> do
      files <- getFilesByProjectID pool pid
      pure $ Right files
