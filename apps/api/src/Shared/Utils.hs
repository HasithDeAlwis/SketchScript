{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  OverloadedStrings #-}

module Shared.Utils where

import Crypto.Random (getRandomBytes)
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Char8 qualified as BS
import Data.String.Conv (StringConv, toS)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Network.URI (parseURI)
import Servant (Handler, ServerError (..), URI, err302, throwError)
import URI.ByteString (Absolute, URIRef, serializeURIRef')

redirects :: (StringConv s BS.ByteString) => s -> Handler ()
redirects url =
  throwError $
    err302
      { errHeaders = [("Location", toS url)],
        errBody = "Redirecting..."
      }

toNetworkURI :: URIRef Absolute -> Maybe Servant.URI
toNetworkURI uriRef =
  Network.URI.parseURI $ BS.unpack $ serializeURIRef' uriRef

genUrlSafe :: IO T.Text
genUrlSafe = do
  bytes <- getRandomBytes 16
  pure $ decodeUtf8 (B64.encodeUnpadded bytes)

hoistMaybe :: ServerError -> Maybe a -> Handler a
hoistMaybe err = maybe (throwError err) pure
