module S3.Handler where

import Data.Text
import Network.AWS (Credentials (Discover), Env, Region (NorthVirginia), newEnv, runAWS, runResourceT)

generatePresignedPutUrl :: Env -> Text -> Text -> IO Text
generatePresignedPutUrl env bucket key = do
  now <- getCurrentTime
  let expiry = addUTCTime (60 * 15) now
  runResourceT . runAWS env $ do
    url <- presignURL expiry $ putObject (BucketName bucket) (ObjectKey key) " "
    return $ toText url

generatePresignedGetUrl :: Env -> Text -> Text -> IO Text
generatePresignedGetUrl env bucket key = do
  now <- getCurrentTime
  let expiry = addUTCTime (60 * 10) now
  runResourceT . runAWS env $ do
    url <- presignURL expiry $ getObject (BucketName bucket) (ObjectKey key)
    return $ toText url

deleteObjectFromS3 :: Env -> Text -> Text -> IO ()
deleteObjectFromS3 env bucket key =
  runResourceT . runAWS env $
    void $
      send $
        deleteObject (BucketName bucket) (ObjectKey key)
