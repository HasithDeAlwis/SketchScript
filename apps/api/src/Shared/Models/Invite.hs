{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.Invite where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Invite = Invite
  { invite_id :: UUID,
    invite_workspace_id :: UUID,
    invite_invited_email :: Text,
    invite_invited_by :: UUID,
    invite_role_id :: UUID,
    invite_token :: Text,
    invite_status :: Text,
    invite_sent_at :: UTCTime,
    invite_expires_at :: UTCTime,
    invite_accepted_at :: Maybe UTCTime
  }
  deriving (Show, Generic)

instance ToJSON Invite
instance FromJSON Invite
instance FromRow Invite
