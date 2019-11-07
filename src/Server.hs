{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Server
  ( runServer
  , myFunction
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Network.Wai.Handler.Warp       ( run )
import           System.Environment             ( getEnv )
import           Servant.API
import           Servant.Server
import           Numeric.Extra                  ( intToDouble )


type MyAPI
  = 
    "api" :> "ping" :> Get '[JSON] String :<|>
    "api" :> "calculate" :> ReqBody '[JSON] [Int] :> Post '[JSON] String

pingHandler :: Handler String
pingHandler = return "ping"

oddsHandler :: [Int] -> Handler String
oddsHandler odds = return $ calculateOdds odds

myAPI :: Proxy MyAPI
myAPI = Proxy :: Proxy MyAPI

myServer :: Server MyAPI
myServer = pingHandler :<|> oddsHandler

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"
  -- let port = 8005
  run port (serve myAPI myServer)

myFunction :: Int -> Int -> Int -> Int
myFunction a b c = a * b + c

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

formatAmerican :: Int -> String
formatAmerican n = if n > 0 then "+" ++ (show n) else show n

calculateOdds :: [Int] -> String
calculateOdds odds = case parseOdds odds of
  Left  str   -> str
  Right odds' -> formatAmerican $ decimalToAmerican $ foldr (*) 1 $ map
    americanToDecimal
    odds'

parseOdds :: [Int] -> Either String [Int]
parseOdds odds =
  if elem 0 odds then Left "Odds cannot contain 0" else Right odds
