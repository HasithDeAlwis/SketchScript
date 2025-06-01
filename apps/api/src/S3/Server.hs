{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module S3.Server where

import Amazonka hiding (length)
import Amazonka.S3
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Generics.Labels ()
import Data.Pool (Pool)
import Data.Text qualified as T (Text, pack)
import Data.Text.Encoding qualified as TE
import Data.Time
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server
import Shared.Models.User (User)
import System.Environment (getEnv)
import System.IO

-- TODO: Refactor into individual files
-- TODO: Add a file naming pattern

-- === FileId ===
newtype FileId = FileId {fileId :: T.Text}
  deriving (Eq, Show, Generic)

instance FromJSON FileId

instance ToJSON FileId

instance FromHttpApiData FileId where
  parseUrlPiece = Right . FileId

getPresignedPutURL ::
  Region ->
  BucketName ->
  ObjectKey ->
  IO ByteString
getPresignedPutURL reg b k = do
  lgr <- newLogger Trace stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg
  ts <- getCurrentTime

  runResourceT $ presignURL env ts 120 (newPutObject b k (toBody ("" :: ByteString)))

getPresignedGetURL :: Region -> BucketName -> ObjectKey -> IO ByteString
getPresignedGetURL reg b k = do
  lgr <- newLogger Trace stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg
  ts <- getCurrentTime

  runResourceT $ presignURL env ts 120 (newGetObject b k)

getPresignedDeleteURL :: Region -> BucketName -> ObjectKey -> IO ByteString
getPresignedDeleteURL reg b k = do
  lgr <- newLogger Trace stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg
  ts <- getCurrentTime

  runResourceT $ presignURL env ts 120 (newDeleteObject b k)

-- === API ===
type API auths =
  Servant.Auth.Server.Auth auths User
    :> ( "s3" :> "upload" :> ReqBody '[JSON] FileId :> Post '[JSON] T.Text
           :<|> "s3" :> "download" :> Capture "fileId" FileId :> Get '[JSON] T.Text
           :<|> "s3" :> Capture "fileId" FileId :> Servant.Delete '[JSON] T.Text
       )

-- === Server ===
server :: Pool Connection -> Server (API auths)
server _dbPool (Authenticated _user) =
  presignUpload :<|> presignDownload :<|> deleteFile
  where
    presignUpload :: FileId -> Handler T.Text
    presignUpload (FileId uuid) = liftIO $ do
      S3Credentials {..} <- getCredentials
      url <- getPresignedPutURL Ohio (BucketName $ T.pack bucketName) (ObjectKey uuid)
      return $ TE.decodeUtf8 url

    presignDownload :: FileId -> Handler T.Text
    presignDownload (FileId uuid) = liftIO $ do
      S3Credentials {..} <- getCredentials
      url <- getPresignedGetURL Ohio (BucketName $ T.pack bucketName) (ObjectKey uuid)
      return $ TE.decodeUtf8 url

    deleteFile :: FileId -> Handler T.Text
    deleteFile (FileId uuid) = liftIO $ do
      S3Credentials {..} <- getCredentials
      url <- getPresignedDeleteURL Ohio (BucketName $ T.pack bucketName) (ObjectKey uuid)
      return $ TE.decodeUtf8 url
server _ _ = throwAll err401 {errBody = "Unauthorized Users"}

-- === AWS Helpers ===
data S3Credentials = S3Credentials
  { publicKey :: String,
    secretKey :: String,
    bucketName :: String,
    regionName :: String
  }

getCredentials :: IO S3Credentials
getCredentials = do
  _ <- loadFile defaultConfig
  public <- getEnv "AWS_ACCESS_KEY"
  secret <- getEnv "AWS_SECRET_KEY"
  bucket <- getEnv "S3_BUCKET_NAME"
  region <- getEnv "S3_REGION"
  return $ S3Credentials public secret bucket region
