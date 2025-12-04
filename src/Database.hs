//database

{-# LANGUAGE OverloadedStrings #-}


module Database
  ( createTables
  , initDatabase
  , insertForce
  , insertCrimeCategory
  , insertCrime
  , getAllForces
  , getAllCrimeCategories
  , getAllCrimes
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as T
import Types
import Data.String (fromString)


-- create and initialize the database



createTables :: Connection -> IO ()
createTables conn = do
  -- Enable foreign keys
  execute_ conn "PRAGMA foreign_keys = ON"

  -- Forces table
  execute_ conn (fromString
    "CREATE TABLE IF NOT EXISTS forces ( \
    \id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \force_id TEXT UNIQUE NOT NULL, \
    \force_name TEXT NOT NULL)")

  -- Categories table
  execute_ conn (fromString
    "CREATE TABLE IF NOT EXISTS crime_categories ( \
    \id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \category_url TEXT UNIQUE NOT NULL, \
    \category_name TEXT NOT NULL)")

  -- Crimes table
  execute_ conn (fromString
    "CREATE TABLE IF NOT EXISTS crimes ( \
    \id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \crime_id TEXT, \
    \month TEXT NOT NULL, \
    \latitude REAL NOT NULL, \
    \longitude REAL NOT NULL, \
    \street_name TEXT, \
    \outcome_status TEXT, \
    \outcome_date TEXT, \
    \force_ref INTEGER, \
    \category_ref INTEGER NOT NULL, \
    \FOREIGN KEY (force_ref) REFERENCES forces(id) ON DELETE SET NULL, \
    \FOREIGN KEY (category_ref) REFERENCES crime_categories(id) ON DELETE CASCADE)")

  putStrLn "database tables created"

------------------------------------------------------------
-- Initialise database
------------------------------------------------------------

-- | Opens database and creates tables
initDatabase :: String -> IO Connection
initDatabase path = do
  conn <- open path
  createTables conn
  return conn

------------------------------------------------------------
-- Insert functions
------------------------------------------------------------

-- | Add one police force
insertForce :: Connection -> Force -> IO ()
insertForce conn (Force fid fname) = do
  execute conn
    "INSERT OR IGNORE INTO forces (force_id, force_name) VALUES (?, ?)"
    (fid, fname)

-- | Add one crime category
insertCrimeCategory :: Connection -> CrimeCategory -> IO ()
insertCrimeCategory conn (CrimeCategory url name) = do
  execute conn
    "INSERT OR IGNORE INTO crime_categories (category_url, category_name) VALUES (?, ?)"
    (url, name)

-- | Add one crime
insertCrime :: Connection -> Crime -> IO ()
insertCrime conn crime = do
  catId <- query conn
    "SELECT id FROM crime_categories WHERE category_url = ?"
    (Only (crimeCategory crime)) :: IO [Only Int]

  case catId of
    [] -> putStrLn "category not found, skipping crime"
    (Only cid : _) -> do
      execute conn
        "INSERT INTO crimes (crime_id, month, latitude, longitude, street_name, outcome_status, outcome_date, force_ref, category_ref) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ( crimeId crime
        , crimeMonth crime
        , crimeLatitude crime
        , crimeLongitude crime
        , crimeStreetName crime
        , crimeOutcomeStatus crime
        , crimeOutcomeDate crime
        , Nothing :: Maybe Int
        , cid
        )

------------------------------------------------------------
-- Query functions
------------------------------------------------------------

-- | Get all police forces
getAllForces :: Connection -> IO [Force]
getAllForces conn =
  query_ conn "SELECT force_id, force_name FROM forces"

-- | Get all crime categories
getAllCrimeCategories :: Connection -> IO [CrimeCategory]
getAllCrimeCategories conn =
  query_ conn "SELECT category_url, category_name FROM crime_categories"

-- | Get all crimes (simple list)
getAllCrimes :: Connection -> IO [(Maybe String, String, Double, Double)]
getAllCrimes conn =
  query_ conn
    "SELECT crime_id, month, latitude, longitude FROM crimes"
