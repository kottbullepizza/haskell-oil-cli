{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs, lookupEnv)
import qualified Data.ByteString.Char8 as B8
import Data.Aeson
import Network.HTTP.Simple

baseURL :: String
baseURL = "https://api.oilpriceapi.com/v1"

-- parsed JSON get

data PriceInfo = PriceInfo
  { price     :: Double
  , formatted :: String
  , currency  :: String
  , code      :: String
  , unit      :: String
  } deriving (Show)

instance FromJSON PriceInfo where
  parseJSON = withObject "PriceInfo" $ \v ->
    PriceInfo
      <$> v .: "price"
      <*> v .: "formatted"
      <*> v .: "currency"
      <*> v .: "code"
      <*> v .: "unit"

data ApiResponse = ApiResponse
  { status :: String
  , data_  :: PriceInfo
  } deriving (Show)

instance FromJSON ApiResponse where
  parseJSON = withObject "ApiResponse" $ \v ->
    ApiResponse
      <$> v .: "status"
      <*> v .: "data"

parsedJSONGet :: String -> String -> IO ()
parsedJSONGet apiKey commodityCode = do
  request <- parseRequest (baseURL ++ "/prices/latest")

  let queriedRequest =
        setRequestQueryString [("by_code", Just (B8.pack commodityCode))] request

  let authenticatedRequest =
        setRequestHeader "Authorization"
          [B8.pack ("Token " ++ apiKey)]
          queriedRequest

  response <- httpJSON authenticatedRequest :: IO (Response ApiResponse)

  let p = data_ (getResponseBody response)
  putStrLn (formatted p ++ " per " ++ unit p)

main :: IO ()
main = do
  maybeKey <- lookupEnv "OILPRICE_API_KEY"
  args <- getArgs

  let commodityCode = case args of
        ["wti"]   -> "WTI_USD"
        ["brent"] -> "BRENT_CRUDE_USD"
        []        -> "BRENT_CRUDE_USD"
        _         -> "BRENT_CRUDE_USD"

  case maybeKey of
    Nothing ->
      fail "Missing OILPRICE_API_KEY environment variable"
    Just apiKey ->
      parsedJSONGet apiKey commodityCode
