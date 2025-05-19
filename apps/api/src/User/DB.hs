{-# LANGUAGE OverloadedStrings #-}

module User.DB where

import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Shared.Models.User

getUserByEmail :: Pool Connection -> Text -> IO [User]
getUserByEmail pool userEmail =
  withResource pool $ \conn ->
    query
      conn
      "SELECT id, email, name, created_at FROM users WHERE email = ?"
      (Only userEmail)

createUser :: Pool Connection -> Text -> Text -> IO User
createUser pool name email = do
  uid <- nextRandom
  now <- getCurrentTime
  withResource pool $ \conn -> do
    [user] <-
      query
        conn
        "INSERT INTO users (id, name, email, created_at) VALUES (?, ?, ?, ?) RETURNING id AS user_id, name AS user_name, email AS user_email, created_at AS user_create_at"
        (uid, name, email, now)
    return user
