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
  , queryCrimesByCategory
  , queryCrimesByMonth
  , getCrimeStats
  , getForceIdByCode
  , updateCrimeForce
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as T
import Types
import Data.String (fromString)


-- Create the three database tables with foreign key relationships
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

  -- Crimes table with foreign keys to forces and categories
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

  putStrLn "Database tables created successfully"


-- Opens database file and creates tables if they don't exist
initDatabase :: String -> IO Connection
initDatabase path = do
  conn <- open path
  createTables conn
  return conn


-- Add one police force to the database
insertForce :: Connection -> Force -> IO ()
insertForce conn (Force fid fname) = do
  execute conn
    "INSERT OR IGNORE INTO forces (force_id, force_name) VALUES (?, ?)"
    (fid, fname)


-- Add one crime category
insertCrimeCategory :: Connection -> CrimeCategory -> IO ()
insertCrimeCategory conn (CrimeCategory url name) = do
  execute conn
    "INSERT OR IGNORE INTO crime_categories (category_url, category_name) VALUES (?, ?)"
    (url, name)


-- Add one crime record
-- Note: This looks up the category by URL to get its ID
insertCrime :: Connection -> Crime -> IO Int
insertCrime conn crime = do
  catId <- query conn
    "SELECT id FROM crime_categories WHERE category_url = ?"
    (Only (crimeCategory crime)) :: IO [Only Int]

  case catId of
    [] -> do
      putStrLn "Warning: Category not found, skipping crime"
      return 0
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
      [Only crimeDbId] <- query_ conn "SELECT last_insert_rowid()" :: IO [Only Int]
      return crimeDbId


-- Get all police forces from database
getAllForces :: Connection -> IO [Force]
getAllForces conn =
  query_ conn "SELECT force_id, force_name FROM forces"


-- Get all crime categories
getAllCrimeCategories :: Connection -> IO [CrimeCategory]
getAllCrimeCategories conn =
  query_ conn "SELECT category_url, category_name FROM crime_categories"


-- Get all crimes as Crime objects
getAllCrimes :: Connection -> IO [Crime]
getAllCrimes conn = do
  results <- query_ conn
    "SELECT crime_id, c.month, c.latitude, c.longitude, c.street_name, \
    \c.outcome_status, c.outcome_date, cc.category_url \
    \FROM crimes c \
    \JOIN crime_categories cc ON c.category_ref = cc.id"
    :: IO [(Maybe T.Text, T.Text, Double, Double, Maybe T.Text, Maybe T.Text, Maybe T.Text, T.Text)]

  return [Crime cid cat mon lat lng sname ostatus odate | (cid, mon, lat, lng, sname, ostatus, odate, cat) <- results]


-- Find crimes by category name
queryCrimesByCategory :: Connection -> T.Text -> IO [Crime]
queryCrimesByCategory conn category = do
  results <- query conn
    "SELECT c.crime_id, c.month, c.latitude, c.longitude, c.street_name, \
    \c.outcome_status, c.outcome_date, cc.category_url \
    \FROM crimes c \
    \JOIN crime_categories cc ON c.category_ref = cc.id \
    \WHERE cc.category_url = ?"
    (Only category)
    :: IO [(Maybe T.Text, T.Text, Double, Double, Maybe T.Text, Maybe T.Text, Maybe T.Text, T.Text)]

  return [Crime cid cat mon lat lng sname ostatus odate | (cid, mon, lat, lng, sname, ostatus, odate, cat) <- results]


-- Find crimes by month
queryCrimesByMonth :: Connection -> T.Text -> IO [Crime]
queryCrimesByMonth conn month = do
  results <- query conn
    "SELECT c.crime_id, c.month, c.latitude, c.longitude, c.street_name, \
    \c.outcome_status, c.outcome_date, cc.category_url \
    \FROM crimes c \
    \JOIN crime_categories cc ON c.category_ref = cc.id \
    \WHERE c.month = ?"
    (Only month)
    :: IO [(Maybe T.Text, T.Text, Double, Double, Maybe T.Text, Maybe T.Text, Maybe T.Text, T.Text)]

  return [Crime cid cat mon lat lng sname ostatus odate | (cid, mon, lat, lng, sname, ostatus, odate, cat) <- results]


-- Get crime statistics grouped by category
-- Returns category name and count
getCrimeStats :: Connection -> IO [(T.Text, Int)]
getCrimeStats conn =
  query_ conn
    "SELECT cc.category_name, COUNT(*) \
    \FROM crimes c \
    \JOIN crime_categories cc ON c.category_ref = cc.id \
    \GROUP BY cc.category_name \
    \ORDER BY COUNT(*) DESC"
-- Get force database ID by force code
getForceIdByCode :: Connection -> T.Text -> IO (Maybe Int)
getForceIdByCode conn forceCode = do
  results <- query conn
    "SELECT id FROM forces WHERE force_id = ?"
    (Only forceCode) :: IO [Only Int]
  case results of
    [] -> return Nothing
    (Only fid : _) -> return (Just fid)
-- Update a crime record to set its force reference
updateCrimeForce :: Connection -> Int -> Maybe Int -> IO ()
updateCrimeForce conn crimeDbId forceDbId = do
  execute conn
    "UPDATE crimes SET force_ref = ? WHERE id = ?"
    (forceDbId, crimeDbId)
