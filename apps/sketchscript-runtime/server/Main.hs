{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Amazonka (ToBody (toBody), discover, newEnv, send)
import Amazonka.Data.Body (ResponseBody (..))
import Amazonka.S3 (BucketName (BucketName), ObjectKey (ObjectKey), newGetObject, newPutObject)
import Amazonka.S3.GetObject (getObjectResponse_body)
import CodeGenerators.Element (genElement)
import Conduit
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Combinators as C
import Data.Generics.Labels ()
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Debug.Trace (traceM)
import GHC.Generics
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Status (internalServerError500)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Parser.Script
import System.Environment (getEnv)
import Web.Scotty (ScottyM, function, jsonData, options, post, scotty, status, text)
import Web.Scotty.Trans (middleware)

data DownloadReq = DownloadReq
  { key :: T.Text
  }
  deriving (Generic, FromJSON)

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  bucketName <- getEnv "S3_BUCKET_NAME"
  origin <- getEnv "CORS_ALLOWED_ORIGIN"
  traceM origin
  scotty 8081 $ do
    middleware $
      addHeaders
        [ ("Access-Control-Allow-Origin", BS.pack origin),
          ("Access-Control-Allow-Headers", "Content-Type, Authorization"),
          ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
        ]
    options (function (const (Just []))) $ do
      status status200
      text ""
    compileEndpoint (BucketName (T.pack bucketName))

compileEndpoint :: BucketName -> ScottyM ()
compileEndpoint defaultBucket = do
  post "/compile" $ do
    DownloadReq {..} <- jsonData
    let objectKey = ObjectKey key
    result <- liftIO $ runExceptT $ do
      content <- downloadFile defaultBucket objectKey
      ast <- case parseSketchScript (T.unpack content) of
        Left err -> throwE err
        Right ast -> return ast
      uploadFile defaultBucket key (concatMap genElement ast)
    case result of
      Left err -> do
        status internalServerError500
        text $ TL.pack err
      Right _ -> text "Successfuly compiled and updated project"

downloadFile :: BucketName -> ObjectKey -> ExceptT String IO T.Text
downloadFile b k = do
  env <- newEnv discover
  result <- liftIO $ try $ runResourceT $ do
    rs <- send env (newGetObject b k)
    runConduit $
      body (rs ^. getObjectResponse_body)
        .| C.decodeUtf8
        .| C.fold
  case result of
    Left (e :: SomeException) -> throwE $ "Failed to download file: " ++ show e
    Right txt -> return txt

uploadFile :: BucketName -> T.Text -> String -> ExceptT String IO T.Text
uploadFile b k s = do
  env <- newEnv discover
  let newKey = k <> ".html"
  let body = toBody (T.pack s)
  result <- liftIO $ try $ runResourceT $ do
    _ <- send env (newPutObject b (ObjectKey newKey) body)
    return ()
  case result of
    Left (e :: SomeException) -> throwE $ "Failed to upload file: " ++ show e
    Right _ -> return "Successfully uploaded file"