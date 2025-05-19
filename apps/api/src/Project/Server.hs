{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Project.Server (API, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.String.Conv (toS)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Project.Handler (getProjectsByWorkspaceIO)
import Servant
import Servant.Auth.Server
import Shared.Models.Project (Project)
import Shared.Models.User (User)

type Protected =
  "project" :> QueryParam "wid" Text :> Get '[JSON] [Project]

type API auths = Auth auths User :> Protected

server :: Pool Connection -> Server (API auths)
server pool (Authenticated _) = getProjectsHandler
  where
    getProjectsHandler :: Maybe Text -> Handler [Project]
    getProjectsHandler (Just widText) = do
      result <- liftIO $ getProjectsByWorkspaceIO pool widText
      case result of
        Left err -> throwError err400 {errBody = toS err}
        Right projects -> pure projects
    getProjectsHandler Nothing =
      throwError err400 {errBody = "Missing wid query parameter"}
server _ _ = throwAll err401
