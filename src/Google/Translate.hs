{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Google.Translate (translate) where

import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)
import Network.Wreq as W
--import Network.Wreq.Lens
--import Data.Map (Map)
import Control.Exception as E
import Network.HTTP.Client as C

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as LBS

-- Data sent to google 

data PostData = PostData {
    q :: String
  , source :: String
  , target :: String
  , format :: String
  } deriving (Show, Generic)

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

translate :: String -> String -> String -> String -> IO (Either String String)
translate key src dest text = do
    eR <- internalPost (PostData text src dest "text") key
    case eR of 
        Left s -> return $ Left s
        Right r ->
            -- TODO get a better error if decode failed
            case (decode (r ^. W.responseBody) :: Maybe BodyData) of 
                Nothing -> return $ Left "can't decode the body..."
                Just v -> return $ Right $ translatedText . head . translations . data' $ v
    where
        internalPost :: PostData -> String -> IO (Either String (Response LBS.ByteString))
        internalPost pd key = do 
            (Right <$> post ("https://translation.googleapis.com/language/translate/v2?key="++key) (toJSON pd)) `E.catch` handler
        handler :: HttpException -> IO (Either String (Response LBS.ByteString))
        handler e@(HttpExceptionRequest _ (StatusCodeException r' _)) =
            return $ Left $ BSC.unpack $ r' ^. W.responseStatus . W.statusMessage 

