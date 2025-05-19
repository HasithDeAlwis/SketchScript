{-# LANGUAGE OverloadedStrings #-}

module User.Handler where

import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Data.UUID (UUID, fromString)
import Shared.Models.User (User (..))

-- Hardcoded UUID helper
uuid :: String -> UUID
uuid = fromJust . fromString

-- Hardcoded UTCTime helper (ISO 8601 string)
time :: String -> UTCTime
time = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

isaac :: User
isaac =
  User
    { user_id = uuid "123e4567-e89b-12d3-a456-426614174000",
      user_name = "Isaac Newton",
      user_email = "isaac@newton.co.uk",
      user_created_at = time "2020-01-01T00:00:00Z"
    }

albert :: User
albert =
  User
    { user_id = uuid "123e4567-e89b-12d3-a456-426614174001",
      user_name = "Albert Einstein",
      user_email = "ae@mc2.org",
      user_created_at = time "2020-01-01T00:00:00Z"
    }

users :: [User]
users = [isaac, albert]
