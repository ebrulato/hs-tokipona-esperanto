{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import Servant.API
import Servant
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import TokiPona.TokiPonaToEsperanto
import TokiPona.Version (version)
import Network.Wai.Handler.Warp
import qualified Google.Translate as GT (translate)
import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO)


keyName = "API_KEY_GOOGLE_TRANSLATE"

type Lat = Int
type Lon = Int
type Lingvo = String

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

data Informoj = Informoj {
  amplekso :: Int,
  versiono :: String
} deriving (Show, Generic)
instance ToJSON Informoj

data TradukaPeto = TradukaPeto {
  tekstoj :: [Text]
} deriving (Show, Generic)
instance FromJSON TradukaPeto

data Traduko = Traduko {
  lingva :: Lingvo,
  kruda :: [Text],
  vortara :: [Text]
} deriving (Show, Generic)
instance ToJSON Traduko

type TokiponaAPI = "tokipona" :> "informu" :> Get '[JSON] Informoj
                  :<|> "tokipona" :> "traduku" :> QueryParam "lingvo" Lingvo :> ReqBody '[JSON] TradukaPeto :> Post '[JSON] [Traduko]


serverTP :: Server TokiponaAPI
serverTP = informu
      :<|> traduku

tokiponaAPI :: Proxy TokiponaAPI
tokiponaAPI = Proxy

informu :: Handler Informoj
informu = return $ Informoj nbCompoundWord version

traduku :: Maybe Lingvo -> TradukaPeto -> Handler [Traduko]
traduku lingvo peto = 
  let
    frazoj = map unpack (tekstoj peto)
    krudaFrazoj = map TokiPona.TokiPonaToEsperanto.translate frazoj
    vortaraFrazoj = map translateWithDico frazoj
    tradukojEO = Traduko "eo" (map pack krudaFrazoj) (map pack vortaraFrazoj)
  in do
    mbk <- liftIO $ lookupEnv keyName
    case (lingvo, mbk) of
      (Nothing, _) -> return [tradukojEO] 
      (Just ling, Nothing) -> throwError err501 { errBody = "translation with Google not supported." }
      (Just ling, Just key) -> do
        lingvaKrudaFrazoj <- liftIO $ mapM (GT.translate key "eo" ling) krudaFrazoj
        lingvaVortaraFrazoj <- liftIO $ mapM (GT.translate key "eo" ling) vortaraFrazoj
        let purigaLingvaKrudaFrazoj = purigas lingvaKrudaFrazoj
        let purigaLingvaVortaraFrazoj = purigas lingvaVortaraFrazoj
        if length purigaLingvaKrudaFrazoj > 0 && length purigaLingvaVortaraFrazoj > 0 then
          return [tradukojEO, Traduko ling (map pack purigaLingvaKrudaFrazoj) (map pack purigaLingvaVortaraFrazoj)]
        else 
          throwError err400 { errBody = "please check the dest language or ask the administrator" }
  where
    purigas :: [Either String String] -> [String]
    purigas l = map (\n -> case n of
      Right s -> s
      Left s -> s) $ filter (\n -> case n of 
      Right _ -> True
      Left _ -> False) l
 

main :: IO ()
main = run 8081 $ serve tokiponaAPI serverTP