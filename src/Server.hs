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
import Web.FormUrlEncoded (FromForm(..), ToForm(..))
import Data.Text (Text, replace, append)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import Text.Read (readMaybe)
import qualified Data.Map as Map

type MyAPI
  = 
    "api" :> "ping" :> ReqBody '[FormUrlEncoded] SlackPayload :> Post '[JSON] SlackResponseMessage :<|>
    "api" :> "calculate" :> ReqBody '[FormUrlEncoded] SlackPayload :> Post '[JSON] SlackResponseMessage :<|>
    "api" :> "football-teaser-payouts"  :> ReqBody '[FormUrlEncoded] SlackPayload :> Post '[JSON] BlockResponse

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

teaserPayoutsHandler :: SlackPayload -> Handler BlockResponse
teaserPayoutsHandler slackPayload = return $ mkTeaserBlockResponse $ mkTeaserPayoutStr (text slackPayload)

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myServer :: Server MyAPI
myServer =  pingHandler :<|> oddsHandler :<|> teaserPayoutsHandler

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

---------------------------------------
-- TEASERS ----------------------------
-- supported teasers 6, 6.5, 7, 10, 13
---------------------------------------

footballTeaserPayouts :: Double -> String
footballTeaserPayouts points = ""

mkTeaserTable = ""

teaserMap :: Map.Map Double [(Int, Int)]
teaserMap =
  Map.fromList 
    [ ( 6
      , [ ( 2, -110 )
        , ( 3, 165 )
        , ( 4, 265 )
        , ( 5, 410 )
        , ( 6, 610 )
        , ( 7, 890 )
        , ( 8, 1275 )
        , ( 9, 1825 )
        , ( 10, 2600 )
        , ( 11, 3700 )
        , ( 12, 5200 )
        , ( 13, 7400 )
        , ( 14, 10500 )
        ]
      )
    , ( 6.5
      , [ ( 2, -120 )
        , ( 3, 150 )
        , ( 4, 240 )
        , ( 5, 365 )
        , ( 6, 550 )
        , ( 7, 800 )
        , ( 8, 1100 )
        , ( 9, 1550 )
        , ( 10, 2150 )
        , ( 11, 2950 )
        , ( 12, 4050 )
        , ( 13, 5600 )
        , ( 14, 7600 )
        ]
      )
      , ( 7
        , [ ( 2, -135 )
          , ( 3, 135 )
          , ( 4, 215 )
          , ( 5, 320 )
          , ( 6, 460 )
          , ( 7, 650 )
          , ( 8, 900 )
          , ( 9, 1250 )
          , ( 10, 1725 )
          , ( 11, 2350 )
          , ( 12, 3200 )
          , ( 13, 4300 )
          , ( 14, 5900 )
          ]
        )
        , ( 10
          , [ ( 2, -210 )
            , ( 3, 110 )
            , ( 4, 128 )
            , ( 5, 180 )
            , ( 6, 245 )
            , ( 7, 325 )
            , ( 8, 425 )
            , ( 9, 550 )
            , ( 10, 710 )
            , ( 11, 915 )
            , ( 12, 1175 )
            , ( 13, 1500 )
            , ( 14, 1900 )
            ]
          )
        , ( 13
          , [ ( 2, -550 )
            , ( 3, -295 )
            , ( 4, -150 )
            , ( 5, -112 )
            , ( 6, 115 )
            , ( 7, 145 )
            , ( 8, 180 )
            , ( 9, 220 )
            , ( 10, 265 )
            , ( 11, 320 )
            , ( 12, 385 )
            , ( 13, 470 )
            , ( 14, 565 )
            ]
          )
    ]

mkTeaserPayoutStr :: Text -> Either Text (Double, Text)
mkTeaserPayoutStr pointsTxt = do
  let
    invalidPointTxt = "*:x: Invalid point value.*\nSupported point values: *6, 6.5, 7, 10, or 13*"
  case readMaybe $ cs pointsTxt :: Maybe Double of
    Nothing -> Left invalidPointTxt
    Just points ->
      case Map.lookup points teaserMap of
        Nothing -> Left invalidPointTxt
        Just lst -> do
          let
            go :: (Int, Int) -> [Text] -> [Text]  
            go (numTeams, odds) lst' = (cs $ "*" `append` (cs $ show numTeams) `append` " Teams:* " `append` (formatAmerican odds)) : lst'
          Right $ (points, T.unlines $ foldr go [] lst)



mkTeaserBlockResponse :: Either Text (Double, Text) -> BlockResponse
mkTeaserBlockResponse resp =
  case resp of
    Left txt -> BlockResponse [ Block "section" $ (Just $ BlockText "mrkdwn" txt) ]
    Right (points, txt) ->
      BlockResponse $
        [ Block "section" $ (Just $ BlockText "mrkdwn" ("*" `append` (cs $ show $ points) `append` " Point Teaser Payouts*"))
        , Block "divider" Nothing
        , Block "section" $ (Just $ BlockText "mrkdwn" txt)
        ]

data BlockResponse = BlockResponse
  { blocks :: [Block]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Block = Block
  { blockType :: Text
  , blockText :: Maybe BlockText
  } deriving (Show, Eq, Generic)

instance ToJSON Block where
  toJSON (Block blockType blockText) = object fields
    where
      consMay attr = maybe id ((:) . (attr .=))
      conss = consMay "text" blockText
      fields = conss ["type" .= blockType]

instance FromJSON Block where
  parseJSON (Object v) = Block
    <$> v .: "type"
    <*> v .: "text"

data BlockText = BlockText
  { blockTextType :: Text
  , blockTextText :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON BlockText where
  toJSON (BlockText blockTextType blockTextText) =
    object ["type" .= blockTextType, "text" .= blockTextText]

instance FromJSON BlockText where
  parseJSON (Object v) = BlockText
    <$> v .: "type"
    <*> v .: "text"