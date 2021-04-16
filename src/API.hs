{-
 The API interface for accessing and pulling RFCs
-}

{-# LANGUAGE OverloadedStrings #-}

module API (
    RFC,
    getAllRFCS,
    rfcToJSON,
    rfcFromJSON
) where

import Data.ByteString.Lazy.Internal as BSL
import Data.ByteString.Lazy (fromStrict)
import Control.Lens ( (^.), (^..) )
import Network.Wreq
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson.Lens ( key, _String )
import Data.Aeson
    ( encode,
      decode,
      Value,
      FromJSON(parseJSON),
      ToJSON(toJSON, toEncoding),
      pairs,
      (.:),
      withObject,
      object,
      KeyValue((.=)), eitherDecode )

data RFC = RFC {
    -- | A single RFC entry has an id, status, date, info provided, and body
      id :: Maybe String
    , status :: String
    , date :: String
    , infoProvided :: String
    , lastUpdated :: String
    , body :: Maybe String -- ^ this could be either Nothing or Just s
    } deriving (Show)

-- | Provide helper methods for converting to and from JSON
rfcToJSON :: RFC -> BSL.ByteString
rfcToJSON = encode

rfcFromJSON :: BSL.ByteString -> Maybe RFC
rfcFromJSON json = decode json :: Maybe RFC

getAllRFCS :: IO ()
getAllRFCS = do
    r <- get "https://local.harry.city/wildpointer/data"
    let body =  r ^.. responseBody . key "rfcs" . _String
    -- Parse using Aeson
    --let json = eitherDecode $ fromStrict . encodeUtf8 $ body :: Either String Value
    --putStrLn ("Body is " ++ show (r ^. responseBody))
    putStrLn $ show body
    --putStrLn $ show json

instance ToJSON RFC where
    -- This generates a Value
    toJSON (RFC id status date infoProvided lastUpdated body) =
        object [
              "id"              .= id
            , "status"          .= status
            , "date"            .= date
            , "info_provided"   .= infoProvided
            , "last_updated"    .= lastUpdated
            , "body"            .= body
        ]

    toEncoding (RFC id status date infoProvided lastUpdated body) =
        pairs (
               "id"             .= id
            <> "status"         .= status
            <> "date"           .= date
            <> "info_provided"  .= infoProvided
            <> "last_updated"   .= lastUpdated
            <> "body"           .= body
        )

instance FromJSON RFC where
    parseJSON = withObject "RFC" $ \v -> RFC
        <$> v .: "id"
        <*> v .: "status"
        <*> v .: "date"
        <*> v .: "info_provided"
        <*> v .: "last_updated"
        <*> v .: "body"
