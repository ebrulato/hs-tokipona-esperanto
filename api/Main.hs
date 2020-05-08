{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import            Data.Aeson                            (ToJSON, FromJSON, encode, toJSON)
import qualified  Data.ByteString.Lazy.Char8    as BL8  (putStr)
import            Data.Text                             (Text, pack, unpack)
import            Data.Time                             (UTCTime)
import            Data.Typeable                         (Typeable)
import            Data.Maybe
import            Data.Swagger
import            Servant.API
import            Servant
import            Servant.Swagger
import            GHC.Generics                          (Generic)
import            Control.Monad.IO.Class                (liftIO)
import            Control.Lens
import            System.Environment                    (lookupEnv)

import            TokiPona.TokiPonaToEsperanto
import qualified  TokiPona.Version              as TPV  (version)
import qualified  Google.Translate              as GT   (translate)

import            Text.Read                             (readMaybe)

import            Network.Wai.Handler.Warp
import            Network.Wai.Middleware.Cors


keyName = "API_KEY_GOOGLE_TRANSLATE"

type Lat = Int
type Lon = Int

data Peto = PetoDeInformoj | PetoDeTraduko deriving (Show, Generic) 

data Uzo = Uzo {
  nomo :: String,
  agxo :: Int,
  loko :: String,
  geo :: (Lat, Lon),
  peto :: Peto,
  petaAmplekso :: Int, 
  lingvo :: Lingvo, 
  dato :: UTCTime
} deriving (Show, Generic) 

-- type for the API 

newtype Lingvo = Lingvo String deriving (Show, Generic, Typeable, FromHttpApiData) 
instance ToJSON Lingvo
instance ToParamSchema Lingvo
instance ToSchema Lingvo where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "An ISO-639-1 valid value supported by Google translate."
    & mapped.schema.example ?~ toJSON (Lingvo "fr")

data Informoj = Informoj {
  amplekso :: Int,
  versiono :: String
} deriving (Show, Generic)
instance ToJSON Informoj
instance ToSchema Informoj where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Give some information about the Tokipona2Eperanto API."
    & mapped.schema.example ?~ toJSON (Informoj nbCompoundWord TPV.version)


data TradukaPeto = TradukaPeto {
  tekstoj :: [Text]
} deriving (Show, Generic)
instance ToJSON TradukaPeto
instance FromJSON TradukaPeto
instance ToSchema TradukaPeto where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A translation request, with a list of phrases in tokipona."
    & mapped.schema.example ?~ toJSON (TradukaPeto ["mi wile moku e kasi kule."])

data Traduko = Traduko {
  lingva :: Lingvo,
  kruda :: [Text],
  vortara :: [Text]
} deriving (Show, Generic)
instance ToJSON Traduko
instance ToSchema Traduko where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A translated version. At least there is the esperanto version, [and an other one if there is no error and if you asked for a Google translation]"
    & mapped.schema.example ?~ toJSON (Traduko (Lingvo "eo") ["mi volas mangxi koloran planto."] ["mi volas mangxi floron."])

-- the API

type ApiInformu = 
  "tokipona" :> "informu" 
  :> Summary "Fetches information about the version of the API like the number of words in the dictionnatry." 
  :> Get '[JSON] Informoj


-- "Get with body" are well supported by servant, but swagger.io and most of the framworks don't
type ApiTraduku = 
  "tokipona" :> "traduku" 
  :> Summary "Used to translate tokipona phrases into esperanto, and if requested into other languages supported by Google." 
  :> QueryParam "lingvo" Lingvo :> ReqBody '[JSON] TradukaPeto 
  :> Post '[JSON] [Traduko]


type TokiponaAPI = 
  ApiInformu 
  :<|> ApiTraduku
   
tokiponaAPI :: Proxy TokiponaAPI
tokiponaAPI = Proxy

-- the services
serverTP :: Server TokiponaAPI
serverTP = informu :<|> traduku

informu :: Handler Informoj
informu = return $ Informoj nbCompoundWord TPV.version

-- FARU : optimumo : se la vortaro faras saman versionon ke la kruda traduko, gxi ne devas peti al Google 
-- FARU : optimumo : limo de peto 
-- FARU : optimumo : se la esperanta traduko havas eraron, gxi ne devus peti al Google novan tradaukon.  
--traduku :: Maybe Lingvo -> TradukaPeto -> Handler (Envelope '[googleTranslateConfError] [Traduko]) 
traduku :: Maybe Lingvo -> TradukaPeto -> Handler [Traduko] 
traduku lingvo peto = 
  let
    frazoj = map unpack (tekstoj peto)
    krudaFrazoj = map TokiPona.TokiPonaToEsperanto.translate frazoj
    vortaraFrazoj = map translateWithDico frazoj
    tradukojEO = Traduko (Lingvo "eo") (map pack krudaFrazoj) (map pack vortaraFrazoj)
  in do
    mbk <- liftIO $ lookupEnv keyName
    case (lingvo, mbk) of
      --(Nothing, _) -> return $ toSuccEnvelope [tradukojEO] 
      (Nothing, _) -> return [tradukojEO] 
      (Just (Lingvo ling), Nothing) -> throwError err501 { errBody = "translation with Google is not supported." }
      (Just (Lingvo ling), Just key) -> do
        lingvaKrudaFrazoj <- liftIO $ mapM (GT.translate key "eo" ling) krudaFrazoj
        lingvaVortaraFrazoj <- liftIO $ mapM (GT.translate key "eo" ling) vortaraFrazoj
        let purigaLingvaKrudaFrazoj = purigas lingvaKrudaFrazoj
        let purigaLingvaVortaraFrazoj = purigas lingvaVortaraFrazoj
        if length purigaLingvaKrudaFrazoj > 0 && length purigaLingvaVortaraFrazoj > 0 then
          --return $ toSuccEnvelope [tradukojEO, Traduko (Lingvo ling) (map pack purigaLingvaKrudaFrazoj) (map pack purigaLingvaVortaraFrazoj)]
          return [tradukojEO, Traduko (Lingvo ling) (map pack purigaLingvaKrudaFrazoj) (map pack purigaLingvaVortaraFrazoj)]
        else 
          throwError err400 { errBody = "please check the dest language or ask the administrator" }
  where
    purigas :: [Either String String] -> [String]
    purigas l = map (\n -> case n of
      Right s -> s
      Left s -> s) $ filter (\n -> case n of 
      Right _ -> True
      Left _ -> False) l

-- the swagger document
swaggerDoc :: String -> Int -> Swagger
swaggerDoc serverName port = toSwagger tokiponaAPI
  & host ?~ (Host (serverName ++ ":" ++ (show port)) Nothing) -- BAD don't understand how to create a Host with a PortNumber
  & schemes ?~ [Http, Https]
  & info.title   .~ "Tokipona 2 Esperanto API"
  & info.version .~ (pack TPV.version)
  & info.description ?~ "This API allowes you to translate a tokipona text into esperanto and at the same time to use Google Translate in order to have a version in your own language."
  & info.license ?~ ("MIT" & url ?~ URL "https://fr.wikipedia.org/wiki/Licence_MIT")

-- To expose the swagger
type WithSwagger api = api :<|> ("swagger.json" :> Get '[JSON] Swagger)

withSwagger :: forall m api application server. Monad m
  => (Proxy (WithSwagger api) -> (server :<|> m Swagger) -> application)  -- serve
  -> Swagger -> Proxy api -> server -> application
withSwagger serve spec _ server =
  serve (Proxy :: Proxy (WithSwagger layout)) (server :<|> return spec)


main :: IO ()
main = do
  -- In some cases we may have to manage to port number... the internal used, and the external...
  port <- getPort
  -- Note, that implies that we know the servername when starting the service... 
  serverName <- getServerName 
  putStrLn $ "Running on "++ serverName ++ ":" ++ (show port)
  run port $ (cors (const policy)) $ withSwagger serve (swaggerDoc serverName port) tokiponaAPI serverTP 
  where
    getPort = fromMaybe 8081 <$> readMaybe <$> fromMaybe "8081" <$> lookupEnv "WAI_PORT"
    getServerName = fromMaybe "localhost" <$> lookupEnv "WAI_SERVER_NAME"
    policy = Just CorsResourcePolicy {
        corsOrigins = Nothing
        , corsMethods = ["GET"]
        , corsRequestHeaders = ["authorization", "content-type"]
        , corsExposedHeaders = Nothing
        , corsMaxAge = Just $ 60*60*24 -- one day
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
      }
