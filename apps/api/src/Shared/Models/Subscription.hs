{-# LANGUAGE DeriveGeneric #-}

module Shared.Models.Subscription where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import GHC.Generics (Generic)

data Subscription = Subscription
  { subscription_id :: UUID,
    subscription_workspace_id :: UUID,
    subscription_plan :: Text,
    subscription_status :: Text,
    subscription_stripe_customer_id :: Text,
    subscription_stripe_subscription_id :: Text,
    subscription_started_at :: UTCTime,
    subscription_ended_at :: Maybe UTCTime
  }
  deriving (Show, Generic)

instance ToJSON Subscription
instance FromJSON Subscription
instance FromRow Subscription
