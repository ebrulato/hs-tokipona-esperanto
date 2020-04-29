{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Google.Translate (translate) where

import Control.Lens ((&), (^.), (^?), (.~), toListOf)
import Data.Aeson
import GHC.Generics (Generic)
import Network.Wreq
import Data.Map (Map)

-- Data sent to google

data PostData = PostData {
    q :: String
  , source :: String
  , target :: String
  , format :: String
  } deriving (Show, Generic)

-- Get GHC to derive a FromJSON instance for us.

instance ToJSON PostData

-- Data returned

data Translated = Translated {
    translatedText :: String
} deriving (Show, Generic)

instance FromJSON Translated

data Translations = Translations {
    translations :: [Translated]
} deriving (Show, Generic)

instance FromJSON Translations

data BodyData = BodyData {
    data' :: Translations
} deriving (Show, Generic)

instance FromJSON BodyData where
  parseJSON (Object v) =
    BodyData <$> v .: "data"

translate :: String -> String -> String -> String -> IO String
translate key src dest text = do
    r <- post ("https://translation.googleapis.com/language/translate/v2?key="++key) (toJSON $ PostData text src dest "text")
    --let rep = asJSON r :: Either E.SomeException (Response BodyData)
    --return $ show rep
    case (decode (r ^. responseBody) :: Maybe BodyData) of 
        Nothing -> return "invalid response"
        Just v -> return $ translatedText . head . translations . data' $ v