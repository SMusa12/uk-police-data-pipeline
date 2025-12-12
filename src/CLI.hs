{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : CLI
Description : Command-line interface logic for UK Police Data Pipeline
Copyright   : (c) Group 3, 2025
License     : MIT
Maintainer  : Group 3(Suhayb Musa, Hamza Lasry, Oussama Djoudi)

This module handles all command-line argument parsing and execution.
-}

module CLI (runCLI) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (forM_, when)
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Database.SQLite.Simple (close, query, Only(..))

import qualified Fetch
import qualified Parse
import qualified Database
import qualified Types
import Types (Crime(..), Force(..), CrimeCategory(..), ForceLocation(..))

-- Run the CLI application
runCLI :: IO ()
runCLI = do
    args <- getArgs
    case args of
        ["create"]    -> cmdCreate
        ["loaddata"]  -> cmdLoadData
        ["dumpdata"]  -> cmdDumpData
        ["crimes-by-category", category] -> cmdCrimesByCategory category
        ["crimes-by-force", forceId]     -> cmdCrimesByForce forceId
        ["crimes-in-month", month]       -> cmdCrimesInMonth month
        ["force-stats"]                  -> cmdForceStats
        _             -> printUsage >> exitFailure

-- Printing information regarding the usage
printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "UK Police Data Pipeline - Group 3"
    , ""
    , "Usage: stack run -- <command> [arguments]"
    , ""
    , "Commands:"
    , "  create                          Create database and tables"
    , "  loaddata                        Download data from UK Police API and save to database"
    , "  dumpdata                        Export all data to data/data.json"
    , ""
    , "Query Commands:"
    , "  crimes-by-category <category>   Query crimes by category (e.g., 'violent-crime')"
    , "  crimes-by-force <force-id>      Query crimes by police force (e.g., 'metropolitan')"
    , "  crimes-in-month <YYYY-MM>       Query crimes in specific month (e.g., '2024-01')"
    , "  force-stats                     Show crime statistics by category"
    , ""
    , "Examples:"
    , "  stack run -- create"
    , "  stack run -- loaddata"
    , "  stack run -- crimes-by-category violent-crime"
    ]

-- Creating the database and tables
cmdCreate :: IO ()
cmdCreate = do
    putStrLn "Creating database and tables..."
    result <- try $ do
        createDirectoryIfMissing True "data"
        conn <- Database.initDatabase "data/police.db"
        close conn
        return ()

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error creating database: " ++ show e
            putStrLn "Database creation failed"
        Right _ -> putStrLn "Database created successfully at data/police.db"

-- Downloading data from the API and saving to database
cmdLoadData :: IO ()
cmdLoadData = do
    putStrLn "Loading data from UK Police API..."

    result <- try $ do
        conn <- Database.initDatabase "data/police.db"

        putStrLn "Fetching police forces..."
        forcesResult <- Fetch.fetchForces
        case forcesResult of
            Left err -> putStrLn $ "Error fetching forces: " ++ show err
            Right forcesJson -> case Parse.parseForces forcesJson of
                Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
                Right forces -> do
                    forM_ forces $ \force -> Database.insertForce conn force
                    putStrLn $ "Saved " ++ show (length forces) ++ " police forces"

        putStrLn "Fetching crime categories..."
        categoriesResult <- Fetch.fetchCrimeCategories
        case categoriesResult of
            Left err -> putStrLn $ "Error fetching categories: " ++ show err
            Right categoriesJson -> case Parse.parseCrimeCategories categoriesJson of
                Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
                Right categories -> do
                    forM_ categories $ \cat -> Database.insertCrimeCategory conn cat
                    putStrLn $ "Saved " ++ show (length categories) ++ " crime categories"

        putStrLn "Fetching crimes from London..."
        crimesResult <- Fetch.fetchCrimesByLocation 51.5074 (-0.1278) "2024-01"
        case crimesResult of
            Left err -> putStrLn $ "Error: " ++ show err
            Right crimesJson -> case Parse.parseCrimes crimesJson of
                Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
                Right crimes -> do
                    forceResult <- Fetch.fetchForceByLocation 51.5074 (-0.1278)
                    forceDbId <- case forceResult of
                        Left _ -> return Nothing
                        Right forceJson -> case Parse.parseForceLocation forceJson of
                            Left _ -> return Nothing
                            Right forceLoc -> Database.getForceIdByCode conn (Types.locForce forceLoc)

                    forM_ crimes $ \crime -> do
                        crimeDbId <- Database.insertCrime conn crime
                        when (crimeDbId > 0) $
                            Database.updateCrimeForce conn crimeDbId forceDbId

                    putStrLn $ "Saved " ++ show (length crimes) ++ " crimes from London"

        putStrLn "Fetching crimes from Manchester..."
        crimes2Result <- Fetch.fetchCrimesByLocation 53.4808 (-2.2426) "2024-01"
        case crimes2Result of
            Left err -> putStrLn $ "Error: " ++ show err
            Right crimesJson -> case Parse.parseCrimes crimesJson of
                Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
                Right crimes -> do
                    forceResult <- Fetch.fetchForceByLocation 53.4808 (-2.2426)
                    forceDbId <- case forceResult of
                        Left _ -> return Nothing
                        Right forceJson -> case Parse.parseForceLocation forceJson of
                            Left _ -> return Nothing
                            Right forceLoc -> Database.getForceIdByCode conn (Types.locForce forceLoc)

                    forM_ crimes $ \crime -> do
                        crimeDbId <- Database.insertCrime conn crime
                        when (crimeDbId > 0) $
                            Database.updateCrimeForce conn crimeDbId forceDbId

                    putStrLn $ "Saved " ++ show (length crimes) ++ " crimes from Manchester"

        close conn
        return ()

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Fatal error during data loading: " ++ show e
            putStrLn "Data loading failed"
        Right _ -> putStrLn "Data loaded successfully"

-- Exporting all data to JSON file
cmdDumpData :: IO ()
cmdDumpData = do
    putStrLn "Exporting data to JSON..."
    result <- try $ do
        conn <- Database.initDatabase "data/police.db"

        crimes <- Database.getAllCrimes conn
        forces <- Database.getAllForces conn
        categories <- Database.getAllCrimeCategories conn

        Parse.writeAllData "data/data.json" crimes forces categories

        close conn
        return (length crimes)

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error exporting data: " ++ show e
            putStrLn "Data export failed"
        Right count -> putStrLn $ "Exported " ++ show count ++ " crimes to data/data.json"

-- Query crimes by category
cmdCrimesByCategory :: String -> IO ()
cmdCrimesByCategory category = do
    putStrLn $ "Querying crimes with category: " ++ category
    result <- try $ do
        conn <- Database.initDatabase "data/police.db"
        results <- Database.queryCrimesByCategory conn (T.pack category)

        putStrLn $ "Found " ++ show (length results) ++ " crimes:"
        forM_ (take 10 results) $ \crime -> do
            putStrLn $ "  - " ++ T.unpack (crimeCategory crime) ++
                      " at (" ++ show (crimeLatitude crime) ++ ", " ++
                      show (crimeLongitude crime) ++ ")"

        when (length results > 10) $
            putStrLn $ "  ... and " ++ show (length results - 10) ++ " more"

        close conn
        return ()

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error querying database: " ++ show e
            putStrLn "Query failed"
        Right _ -> putStrLn "Query complete"

-- Query crimes by force
cmdCrimesByForce :: String -> IO ()
cmdCrimesByForce forceId = do
    putStrLn $ "Querying crimes for force: " ++ forceId
    result <- try $ do
        conn <- Database.initDatabase "data/police.db"

        forceDbId <- Database.getForceIdByCode conn (T.pack forceId)

        case forceDbId of
            Nothing -> putStrLn "Force not found"
            Just fid -> do
                results <- query conn
                    "SELECT c.crime_id, c.month, c.latitude, c.longitude, c.street_name, \
                    \c.outcome_status, c.outcome_date, cc.category_url \
                    \FROM crimes c \
                    \JOIN crime_categories cc ON c.category_ref = cc.id \
                    \WHERE c.force_ref = ?"
                    (Only fid)
                    :: IO [(Maybe T.Text, T.Text, Double, Double, Maybe T.Text, Maybe T.Text, Maybe T.Text, T.Text)]

                let crimes = [Crime cid cat mon lat lng sname ostatus odate | (cid, mon, lat, lng, sname, ostatus, odate, cat) <- results]

                putStrLn $ "Found " ++ show (length crimes) ++ " crimes for force " ++ forceId
                forM_ (take 10 crimes) $ \crime -> do
                    putStrLn $ "  - " ++ T.unpack (crimeCategory crime) ++ " in " ++ T.unpack (crimeMonth crime)

                when (length crimes > 10) $
                    putStrLn $ "  ... and " ++ show (length crimes - 10) ++ " more"

        close conn
        return ()

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error querying database: " ++ show e
            putStrLn "Query failed"
        Right _ -> putStrLn "Query complete"

-- Query crimes by month
cmdCrimesInMonth :: String -> IO ()
cmdCrimesInMonth month = do
    putStrLn $ "Querying crimes in month: " ++ month
    result <- try $ do
        conn <- Database.initDatabase "data/police.db"
        results <- Database.queryCrimesByMonth conn (T.pack month)

        putStrLn $ "Found " ++ show (length results) ++ " crimes in " ++ month
        forM_ (take 10 results) $ \crime -> do
            putStrLn $ "  - " ++ T.unpack (crimeCategory crime) ++
                      " in " ++ T.unpack (crimeMonth crime)

        when (length results > 10) $
            putStrLn $ "  ... and " ++ show (length results - 10) ++ " more"

        close conn
        return ()

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error querying database: " ++ show e
            putStrLn "Query failed"
        Right _ -> putStrLn "Query complete"

-- Show crime statistics
cmdForceStats :: IO ()
cmdForceStats = do
    putStrLn "Generating crime statistics..."
    result <- try $ do
        conn <- Database.initDatabase "data/police.db"
        stats <- Database.getCrimeStats conn

        putStrLn "Crime Statistics by Category:"
        forM_ stats $ \(category, count) ->
            putStrLn $ T.unpack category ++ ": " ++ show count
        putStrLn $ "Total crimes: " ++ show (sum $ map snd stats)

        close conn
        return ()

    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error generating statistics: " ++ show e
            putStrLn "Statistics generation failed"
        Right _ -> putStrLn "Statistics generated"
