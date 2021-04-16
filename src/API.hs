{-
 The API interface for accessing and pulling RFCs
-}

{-# LANGUAGE OverloadedStrings #-}

module API (
    RFC,
    getAllRFCS,
    printRFC,
    rfcToJSON,
    rfcFromJSON
) where

import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.HashMap.Strict as HM

import Control.Lens ((^.))
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (fromStrict)
import Network.Wreq
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson
    ( object,
      withObject,
      fromJSON,
      (.:), (.:?),
      pairs,
      decode,
      encode,
      KeyValue((.=)),
      Result(Success),
      Value(Array, Object),
      FromJSON(parseJSON),
      ToJSON(toJSON, toEncoding) )

data RFC = RFC {
    -- | A single RFC entry has an id, status, date, info provided, and body
      id' :: String
    , status :: String
    , date :: String
    , infoProvided :: String
    , lastUpdated :: Maybe String
    , body :: Maybe String -- ^ this could be either Nothing or Just s
    } deriving (Show)

-- | Provide helper methods for converting to and from JSON
rfcToJSON :: RFC -> BSL.ByteString
rfcToJSON = encode

rfcFromJSON :: BSL.ByteString -> Maybe RFC
rfcFromJSON json = decode json :: Maybe RFC

getAllRFCS :: IO [RFC]
getAllRFCS = do
    r <- get "https://local.harry.city/wildpointer/data"
    let body = r ^. responseBody

    -- Hack until application/json is fixed
    let cbt = decode body :: Maybe Value

    let rfcs = case cbt of
            Just (Object o) -> case HM.lookup "rfcs" o of
                Just (Array xs) -> (case traverse fromJSON $ toList xs of
                    Success rs -> rs) :: [RFC]
    return rfcs

-- | Print an RFC entry out
printRFC :: RFC -> IO ()
printRFC rfc = do
    let updated = fromMaybe "Never" $ lastUpdated rfc

    putStrLn ("ID: " ++ id' rfc)
    putStrLn ("Status: " ++ status rfc)
    putStrLn ("Date: " ++ date rfc)
    putStrLn ("Info Provided: " ++ infoProvided rfc)
    putStrLn ("Last Updated: " ++ updated)

    case body rfc of 
        Just b  -> putStrLn ("Body: " ++ b)
        Nothing -> putStrLn ""

    putStrLn "\n"

instance ToJSON RFC where
    -- This generates a Value
    toJSON (RFC id' status date infoProvided lastUpdated body) =
        object [
              "id"              .= id'
            , "status"          .= status
            , "date"            .= date
            , "info_provided"   .= infoProvided
            , "last_updated"    .= lastUpdated
            , "body"            .= body
        ]

    toEncoding (RFC id' status date infoProvided lastUpdated body) =
        pairs (
               "id"             .= id'
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
        <*> v .:? "last_updated"
        <*> v .:? "body"
