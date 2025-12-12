{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Crime(..)
  , Force(..)
  , CrimeCategory(..)
  , ForceLocation(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- Represents a UK police force
data Force = Force
  { forceId :: T.Text
  , forceName :: T.Text
  } deriving (Show, Generic, Eq)

-- Parse force data from the API JSON response
instance FromJSON Force where
  parseJSON = withObject "Force" $ \v -> Force
    <$> v .: "id"
    <*> v .: "name"

-- Convert force to JSON for export
instance ToJSON Force

-- Read force from database row
instance FromRow Force where
  fromRow = Force <$> field <*> field

-- Write force to database row
instance ToRow Force where
  toRow (Force fid fname) = toRow (fid, fname)


-- Represents a crime category type
data CrimeCategory = CrimeCategory
  { categoryUrl :: T.Text
  , categoryName :: T.Text
  } deriving (Show, Generic, Eq)

-- Parse category data from API
instance FromJSON CrimeCategory where
  parseJSON = withObject "CrimeCategory" $ \v -> CrimeCategory
    <$> v .: "url"
    <*> v .: "name"

instance ToJSON CrimeCategory

instance FromRow CrimeCategory where
  fromRow = CrimeCategory <$> field <*> field

instance ToRow CrimeCategory where
  toRow (CrimeCategory url name) = toRow (url, name)

-- Represents the force location
data ForceLocation = ForceLocation
  { locForce :: T.Text
  } deriving (Show, Generic, Eq)

instance FromJSON ForceLocation where
  parseJSON = withObject "ForceLocation" $ \v -> ForceLocation
    <$> v .: "force"



-- Represents a single crime record
data Crime = Crime
  { crimeId :: Maybe T.Text
  , crimeCategory :: T.Text
  , crimeMonth :: T.Text
  , crimeLatitude :: Double
  , crimeLongitude :: Double
  , crimeStreetName :: Maybe T.Text
  , crimeOutcomeStatus :: Maybe T.Text
  , crimeOutcomeDate :: Maybe T.Text
  } deriving (Show, Generic, Eq)

-- Parse crime data from the UK Police API JSON
-- The API returns nested JSON so we need to extract location and outcome info
instance FromJSON Crime where
  parseJSON = withObject "Crime" $ \v -> do
    cid <- v .:? "persistent_id"
    cat <- v .: "category"
    mon <- v .: "month"

    -- Location data is nested in its own object
    location <- v .: "location"
    lat <- location .: "latitude" >>= parseCoord
    lng <- location .: "longitude" >>= parseCoord

    -- Street info is also nested
    street <- location .: "street"
    sname <- street .:? "name"

    -- Outcome data might not exist for all crimes
    outcome <- v .:? "outcome_status"
    ostatus <- case outcome of
      Just o -> o .:? "category"
      Nothing -> return Nothing
    odate <- case outcome of
      Just o -> o .:? "date"
      Nothing -> return Nothing

    return $ Crime cid cat mon lat lng sname ostatus odate
    where
      -- Helper to convert text coordinates to doubles
      parseCoord :: T.Text -> Parser Double
      parseCoord t = case reads (T.unpack t) of
        [(n, "")] -> return n
        _ -> fail "Invalid coordinate"

instance ToJSON Crime

-- Database row mapping for crimes table
instance FromRow Crime where
  fromRow = Crime
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

instance ToRow Crime where
  toRow (Crime cid cat mon lat lng sname ostatus odate) =
    toRow (cid, cat, mon, lat, lng, sname, ostatus, odate)
