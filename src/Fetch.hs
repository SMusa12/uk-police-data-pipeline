{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Fetch
Description : HTTP client for UK Police API
Copyright   : (c) Group 3, 2025
License     : MIT
Maintainer  : Group 3(Suhayb Musa, Hamza Lasry, Oussama Djoudi)

This module handles all HTTP requests to the UK Police Data API.
-}

module Fetch
    ( fetchForces
    , fetchCrimesByLocation
    , fetchCrimesByForce
    , fetchCrimeCategories
    , fetchForceByLocation
    , APIError(..)
    , testFetch
    ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)

-- | Custom error types for API operations
data APIError
    = NetworkError String      -- ^ Network connection failed
    | ParseError String        -- ^ JSON parsing failed
    | APIRateLimitError        -- ^ Hit rate limit
    | APINotFoundError         -- ^ Resource not found (404)
    deriving (Show, Eq)

-- | Base URL for UK Police API
baseURL :: String
baseURL = "https://data.police.uk/api"

-- | Fetch list of all police forces
-- Returns raw JSON as ByteString
fetchForces :: IO (Either APIError BL.ByteString)
fetchForces = do
    putStrLn "Fetching police forces..."
    makeRequest (baseURL ++ "/forces")

-- | Fetch all crime categories
fetchCrimeCategories :: IO (Either APIError BL.ByteString)
fetchCrimeCategories = do
    putStrLn "Fetching crime categories..."
    makeRequest (baseURL ++ "/crime-categories")

-- | Fetch crimes at a specific location
-- Takes latitude, longitude, and date (YYYY-MM format)
fetchCrimesByLocation :: Double -> Double -> String -> IO (Either APIError BL.ByteString)
fetchCrimesByLocation lat lng date = do
    putStrLn $ "Fetching crimes at location (" ++ show lat ++ ", " ++ show lng ++ ") for " ++ date
    let url = baseURL ++ "/crimes-street/all-crime?lat=" ++ show lat
                    ++ "&lng=" ++ show lng
                    ++ "&date=" ++ date
    makeRequest url

-- | Fetch crimes for a specific police force
-- Takes force ID and date (YYYY-MM format)
fetchCrimesByForce :: String -> String -> IO (Either APIError BL.ByteString)
fetchCrimesByForce forceId date = do
    putStrLn $ "Fetching crimes for force: " ++ forceId ++ " in " ++ date
    let url = baseURL ++ "/crimes-no-location?category=all-crime&force="
                ++ forceId ++ "&date=" ++ date
    makeRequest url
-- Fetching the police force responsible for a specific location
fetchForceByLocation :: Double -> Double -> IO (Either APIError BL.ByteString)
fetchForceByLocation lat lng = do
    putStrLn $ "Fetching police force for location (" ++ show lat ++ ", " ++ show lng ++ ")"
    let url = baseURL ++ "/locate-neighbourhood?q=" ++ show lat ++ "," ++ show lng
    makeRequest url
-- | Make an HTTP request with error handling
makeRequest :: String -> IO (Either APIError BL.ByteString)
makeRequest url = do
    result <- try $ httpLBS (parseRequest_ url)
    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Network error: " ++ show e
            return $ Left (NetworkError $ show e)
        Right response -> do
            let status = getResponseStatusCode response
            case status of
                200 -> do
                    putStrLn "Request successful"
                    -- Add delay to respect rate limits (100ms between requests)
                    threadDelay 100000
                    return $ Right (getResponseBody response)
                404 -> do
                    putStrLn "Resource not found (404)"
                    return $ Left APINotFoundError
                429 -> do
                    putStrLn "Rate limit exceeded (429)"
                    return $ Left APIRateLimitError
                _ -> do
                    putStrLn $ "HTTP error: " ++ show status
                    return $ Left (NetworkError $ "HTTP " ++ show status)

-- | Helper function to test fetch in isolation
testFetch :: IO ()
testFetch = do
    putStrLn "\n=== Testing UK Police API Fetch ==="

    -- Test 1: Fetch forces
    putStrLn "\nTest 1: Fetching all forces..."
    forcesResult <- fetchForces
    case forcesResult of
        Left err -> putStrLn $ "Failed: " ++ show err
        Right json -> putStrLn $ "Success! Got " ++ show (BL.length json) ++ " bytes"

    -- Test 2: Fetch crimes by location (London)
    putStrLn "\nTest 2: Fetching crimes in London..."
    crimesResult <- fetchCrimesByLocation 51.5074 (-0.1278) "2024-01"
    case crimesResult of
        Left err -> putStrLn $ "Failed: " ++ show err
        Right json -> putStrLn $ "Success! Got " ++ show (BL.length json) ++ " bytes"

    -- Test 3: Fetch crimes by force (Metropolitan Police)
    putStrLn "\nTest 3: Fetching crimes for Metropolitan Police..."
    forceResult <- fetchCrimesByForce "metropolitan" "2024-01"
    case forceResult of
        Left err -> putStrLn $ "Failed: " ++ show err
        Right json -> putStrLn $ "Success! Got " ++ show (BL.length json) ++ " bytes"

    putStrLn "\n=== All tests complete ==="
