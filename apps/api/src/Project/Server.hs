{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Project.Server (API, server) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as BL
import Data.Pool (Pool)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.Simple (Connection)
import Project.Handler (ProjectNameUpdate (..), getProjectsByWorkspaceIO, updateProjectNameIO)
import Servant
import Servant.Auth.Server
import Shared.Models.Project (Project)
import Shared.Models.User (User)

type Protected =
  "project"
    :> QueryParam "wid" Text
    :> Get '[JSON] [Project]
    :<|> "project"
      :> Capture "id" Text
      :> ReqBody '[JSON] ProjectNameUpdate
      :> Put '[JSON] Project

type API auths = Auth auths User :> Protected

server :: Pool Connection -> Server (API auths)
server pool (Authenticated _user) = getProjectsHandler :<|> updateProjectNameHandler
  where
    -- GET /project?wid=...
    getProjectsHandler :: Maybe Text -> Handler [Project]
    getProjectsHandler (Just widText) = do
      result <- liftIO $ getProjectsByWorkspaceIO pool widText
      case result of
        Left err -> throwError err400 {errBody = toS err}
        Right projects -> pure projects
    getProjectsHandler Nothing =
      throwError err400 {errBody = "Missing wid query parameter"}

    -- PUT /project/:id with { "naMe": "..." }
    updateProjectNameHandler :: Text -> ProjectNameUpdate -> Handler Project
    updateProjectNameHandler pid (ProjectNameUpdate newName) = do
      result <- liftIO $ updateProjectNameIO pool pid newName
      case result of
        Left err -> throwError err404 {errBody = BL.fromStrict $ TE.encodeUtf8 err}
        Right updated -> pure updated
server _ _ = throwAll err401 {errBody = "Unauthorized"}
