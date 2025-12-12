{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseCrimes
  , parseForces
  , parseCrimeCategories
  , parseForceLocation
  , writeCrimes
  , writeAllData
  ) where

import Types (Crime, Force, CrimeCategory, ForceLocation)
import Data.Aeson ( eitherDecode, encode, object, (.=) )
import qualified Data.ByteString.Lazy as B


-- Parse a list of crimes from the API JSON response
parseCrimes :: B.ByteString -> Either String [Crime]
parseCrimes = eitherDecode


-- Parse police forces from the API response
parseForces :: B.ByteString -> Either String [Force]
parseForces = eitherDecode
-- parse force location
parseForceLocation :: B.ByteString -> Either String ForceLocation
parseForceLocation = eitherDecode




-- Parse crime categories from the API response
parseCrimeCategories :: B.ByteString -> Either String [CrimeCategory]
parseCrimeCategories = eitherDecode


--  writing the crimes to a JSON file
writeCrimes :: FilePath -> [Crime] -> IO ()
writeCrimes fp crimes = B.writeFile fp (encode crimes)


-- Write all data to JSON for the dumpdata command
-- Combines crimes, forces, and categories into one file
writeAllData :: FilePath -> [Crime] -> [Force] -> [CrimeCategory] -> IO ()
writeAllData fp crimes forces categories = do
  let allData = object
        [ "crimes" .= crimes
        , "forces" .= forces
        , "categories" .= categories
        ]
  B.writeFile fp (encode allData)
