{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module S3.Server where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 (pack)
import Data.Pool (Pool)
import Data.Text qualified as T (Text, intercalate, pack, unpack)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Network.S3 qualified as NetworkS3
import Servant
import Servant.Auth.Server
import Shared.Models.User (User)
import System.Environment (getEnv)

-- TODO: Refactor into individual files
-- TODO: Add a file naming pattern

-- === FileId ===
newtype FileId = FileId {fileId :: UUID.UUID}
  deriving (Eq, Show, Generic)

instance FromJSON FileId

instance ToJSON FileId

instance FromHttpApiData FileId where
  parseUrlPiece t = case UUID.fromText t of
    Just uuid -> Right (FileId uuid)
    Nothing -> Left $ T.pack $ "Invalid UUID: " <> T.unpack t

----- PARSING FOR THE S3SIGNED RESPONSE ----------
data S3SignedResponse = S3SignedResponse
  { sigHeaders :: [(T.Text, T.Text)],
    sigDate :: T.Text,
    sigCredential :: T.Text,
    sigPolicy :: T.Text,
    sigSignature :: T.Text,
    sigAlgorithm :: T.Text
  }
  deriving (Generic)

fromS3SignedRequest :: NetworkS3.S3SignedRequest -> S3SignedResponse
fromS3SignedRequest NetworkS3.S3SignedRequest {..} =
  S3SignedResponse
    { sigHeaders = map (bimap decode decode) (map NetworkS3.getS3Header sigHeaders),
      sigDate = decode sigDate,
      sigCredential = decode sigCredential,
      sigPolicy = decode sigPolicy,
      sigSignature = decode sigSignature,
      sigAlgorithm = decode sigAlgorithm
    }
  where
    decode = TE.decodeUtf8

instance ToJSON S3SignedResponse

--------------------------------------------------------------

-- === API ===
type API auths =
  Auth auths User
    :> ( "s3" :> "upload" :> ReqBody '[JSON] FileId :> Post '[JSON] T.Text
           :<|> "s3" :> "download" :> Capture "fileId" FileId :> Get '[JSON] T.Text
           :<|> "s3" :> Capture "fileId" FileId :> Delete '[JSON] T.Text
       )

-- === Server ===
server :: Pool Connection -> Server (API auths)
server _dbPool (Authenticated _user) =
  presignUpload :<|> presignDownload :<|> deleteFile
  where
    presign :: NetworkS3.S3Method -> FileId -> Handler T.Text
    presign method (FileId uuid) = liftIO $ do
      S3Credentials {..} <- getCredentials
      let bucket = T.pack bucketName
          region = T.pack regionName
          objectKey = T.pack $ UUID.toString uuid
      signed <- generatePresignedURL method uuid
      return $ buildS3URL bucket region objectKey signed

    presignUpload = presign NetworkS3.S3PUT
    presignDownload = presign NetworkS3.S3GET
    deleteFile = presign NetworkS3.S3DELETE
server _ _ = throwAll err401 {errBody = "Unauthorized User"}

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

generatePresignedURL :: NetworkS3.S3Method -> UUID.UUID -> IO NetworkS3.S3SignedRequest
generatePresignedURL method fileId = do
  S3Credentials {..} <- getCredentials
  now <- getCurrentTime
  let objectName' = pack (UUID.toString fileId)
      request =
        NetworkS3.S3Request
          { s3method = method,
            mimeType = Just "application/octet-stream",
            bucketName = pack bucketName,
            objectName = objectName',
            regionName = pack regionName,
            queryString = [],
            requestTime = now,
            payloadHash = Nothing,
            s3headers = []
          }
  NetworkS3.generateS3URL (pack secretKey) request

buildS3URL :: T.Text -> T.Text -> T.Text -> NetworkS3.S3SignedRequest -> T.Text
buildS3URL bucket region objectKey NetworkS3.S3SignedRequest {..} =
  let baseUrl =
        "https://" <> bucket <> ".s3." <> region <> ".amazonaws.com/" <> objectKey

      queryParams =
        [ ("X-Amz-Algorithm", TE.decodeUtf8 sigAlgorithm),
          ("X-Amz-Credential", TE.decodeUtf8 sigCredential),
          ("X-Amz-Date", TE.decodeUtf8 sigDate),
          ("X-Amz-Signature", TE.decodeUtf8 sigSignature)
        ]

      queryString =
        T.intercalate "&" $
          map (\(k, v) -> k <> "=" <> v) queryParams
   in baseUrl <> "?" <> queryString
