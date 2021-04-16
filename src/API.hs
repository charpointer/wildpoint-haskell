{-
 The API interface for accessing and pulling RFCs
-}

-- Enable the DeriveGeneric language feature so our RFC structure is serializable
-- by Aeson
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API (
    RFC,
    rfcToJSON,
    rfcFromJSON
) where

import GHC.Generics (Generic)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Aeson
    ( defaultOptions,
      genericToEncoding,
      decode,
      encode,
      FromJSON,
      ToJSON,
      toJSON,
      parseJSON,
      withObject,
      object,
      (.=),
      (.:),
       )

data RFC = RFC {
    -- | A single RFC entry has an id, status, date, info provided, and body
      id :: String
    , status :: String
    , date :: String
    , infoProvided :: String
    , lastUpdated :: String
    , body :: Maybe String 
    } deriving (Show) -- derive from Generic so we can use Aeson

-- | Provide helper methods for converting to and from JSON
rfcToJSON :: RFC -> ByteString
rfcToJSON = encode

rfcFromJSON :: ByteString -> Maybe RFC
rfcFromJSON json = decode json :: Maybe RFC

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

instance FromJSON RFC where
    parseJSON = withObject "RFC" $ \v -> RFC
        <$> v .: "id"
        <*> v .: "status"
        <*> v .: "date"
        <*> v .: "info_provided"
        <*> v .: "last_updated"
        <*> v .: "body"
