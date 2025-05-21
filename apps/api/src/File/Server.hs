{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module File.Server (API, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import File.Handler
import File.Types
import Servant
import Servant.Auth.Server
import Shared.Models.File (File)
import Shared.Models.User (User)

type Protected =
  "file"
    :> ( QueryParam "pid" Text :> Get '[JSON] [File]
           :<|> ReqBody '[JSON] CreateFileReq :> Post '[JSON] FileResponse
           :<|> ReqBody '[JSON] UpdateFileReq :> Put '[JSON] FileResponse
           :<|> Capture "fileId" UUID :> Delete '[JSON] NoContent
       )

type API auths = Auth auths User :> Protected

server :: Pool Connection -> Server (API auths)
server pool (Authenticated _) =
  getFilesHandler
    :<|> createHandler
    :<|> updateHandler
    :<|> deleteHandler
  where
    createHandler = liftIO . createFileHandler pool
    updateHandler = liftIO . updateFileHandler pool
    deleteHandler fileId = liftIO (deleteFileHandler pool fileId) >> pure NoContent
    getFilesHandler (Just pidText) = do
      result <- liftIO $ getFilesByProjectIO pool pidText
      case result of
        Left err -> throwError err400 {errBody = toS err}
        Right files -> pure files
    getFilesHandler Nothing =
      throwError err400 {errBody = "Missing pid query parameter"}
server _ _ = throwAll err401 {errBody = "Unauthorized User"}
