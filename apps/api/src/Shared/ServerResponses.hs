{-# LANGUAGE DeriveGeneric #-}

module Shared.ServerResponses where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- import Servant.Auth.Server (FromJWT, ToJWT)

-- Error wrapper
data Error a = Error
  { code :: String,
    errMessage :: String,
    details :: Maybe a
  }
  deriving (Generic, Show)

instance (ToJSON a) => ToJSON (Error a)

instance (FromJSON a) => FromJSON (Error a)
