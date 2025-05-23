{-# LANGUAGE DeriveGeneric #-}

module File.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data CreateFileReq = CreateFileReq
  { createReqProjectId :: UUID,
    createReqParentId :: Maybe UUID,
    createReqName :: Maybe Text,
    createReqFileType :: Text
  }
  deriving (Generic)

instance FromJSON CreateFileReq

data UpdateFileReq = UpdateFileReq
  { updateReqFileId :: UUID,
    updateReqProjectId :: UUID,
    updateReqParentId :: Maybe UUID,
    updateReqName :: Maybe Text
  }
  deriving (Generic)

instance FromJSON UpdateFileReq

data FileResponse = FileResponse
  { resFileID :: UUID,
    resFileName :: Text,
    resFileParent :: Maybe UUID
  }
  deriving (Generic)

instance ToJSON FileResponse
