{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module Server
  ( runServer
  )
where

import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import Servant.API
import Servant.Server
import Numeric.Extra (intToDouble)
import GHC.Generics (Generic)
import Data.Aeson
import Data.Monoid
import Servant
import Web.FormUrlEncoded(FromForm(..), ToForm(..))
import Data.Text (Text, replace, append)
import Data.String.Conversions (cs)
import Text.Read (readMaybe)

type MyAPI
  = 
    "api" :> "ping" :> ReqBody '[FormUrlEncoded] SlackPayload :> Post '[JSON] SlackResponseMessage :<|>
    "api" :> "calculate" :> ReqBody '[FormUrlEncoded] SlackPayload :> Post '[JSON] SlackResponseMessage

mkSlackResponseMessage :: Text -> SlackResponseMessage
mkSlackResponseMessage =
  SlackResponseMessage "in_channel"

pingHandler :: SlackPayload -> Handler SlackResponseMessage
pingHandler _ = return $ mkSlackResponseMessage "ping"

oddsHandler :: SlackPayload -> Handler SlackResponseMessage
oddsHandler slackPayload = return $ mkSlackResponseMessage $
  case parseOdds (text slackPayload) of
    Left str -> str
    Right odds -> calculateOdds odds

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myServer :: Server MyAPI
myServer =  pingHandler :<|> oddsHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  -- let port = 8005
  run port (serve myAPI myServer)

data SlackPayload = SlackPayload
  { text :: Text
  } deriving (Eq, Show, Generic)

instance ToForm SlackPayload

instance FromForm SlackPayload

data SlackResponseMessage = SlackResponseMessage
  { response_type :: Text
  , response_text :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON SlackResponseMessage where
  toJSON (SlackResponseMessage response_type response_text) =
    object ["response_type" .= response_type, "text" .= response_text]

instance FromJSON SlackResponseMessage where
  parseJSON (Object v) = SlackResponseMessage
    <$> v .: "response_type"
    <*> v .: "text"
    
-- Calculator Stuff --

testVals :: [Int]
testVals = [-110, 150, -160, 100]

decimalToAmerican :: Double -> Int
decimalToAmerican x = if x >= 2
  then round $ round5dp $ (x - 1) * 100
  else round $ -100 / (round5dp x - 1)

americanToDecimal :: Int -> Double
americanToDecimal x =
  let d = intToDouble x
      n = if x > 0 then d / 100 + 1 else ((-1 * d) + 100) / d * (-1)
  in  round5dp n

round5dp :: Double -> Double
round5dp x = fromIntegral (round $ x * 1e5) / 1e5

formatAmerican :: Int -> Text
formatAmerican n = if n > 0 then "+" `append` (cs $ show n) else cs $ show n

calculateOdds :: [Int] -> Text
calculateOdds odds =
  formatAmerican $ decimalToAmerican $ foldr (*) 1 $ map
    americanToDecimal
    odds

parseOdds :: Text -> Either Text [Int]
parseOdds txt =
  case readMaybe $ cs $ replace "+" "" txt of
    Nothing -> Left "Could not parse odds"
    Just odds -> if elem 0 odds then Left "Odds cannot contain 0" else Right odds
