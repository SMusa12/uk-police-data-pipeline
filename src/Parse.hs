{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseCrimes
  , writeCrimes
  ) where

import Types

import Data.Aeson
  ( eitherDecode
  , encode
  )
import qualified Data.ByteString.Lazy as B


parseCrimes :: B.ByteString -> Either String [Crime]
parseCrimes = eitherDecode


writeCrimes :: FilePath -> [Crime] -> IO ()
writeCrimes fp crimes = B.writeFile fp (encode crimes)
