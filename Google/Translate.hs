{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Google.Translate (translate) where

import Control.Lens ((&), (^.), (^?), (.~))
import Data.Aeson.Lens (key)
import Data.Aeson (FromJSON, ToJSON, toJSON, decode, Value)
import GHC.Generics (Generic)
import Network.Wreq
import Data.Map (Map)
import Data.Text (Text)

-- Data sent to google

data PostData = PostData {
    q :: String
  , source :: String
  , target :: String
  , format :: String
  } deriving (Show, Generic)

-- Get GHC to derive a FromJSON instance for us.

instance FromJSON PostData
instance ToJSON PostData


translate :: String -> String -> String -> String -> IO String
translate key src dest text = do
    r <- post ("https://translation.googleapis.com/language/translate/v2?key="++key) (toJSON $ PostData text src dest "text")
    --return $ show $ r ^. responseHeader "Content-Type"
    case (decode (r ^. responseBody) :: Maybe Value) of 
        Nothing -> return "invalid response"
        Just v -> return $ show $ v ^. key "data"