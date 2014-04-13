{-# LANGUAGE DeriveDataTypeable #-}
module Main
   where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as A
import Control.Applicative (optional)
import Control.Monad       (msum, mzero)
import Control.Monad.Trans (liftIO)
import Control.Exception   (bracket)
import Data.Acid           (openLocalState)
import Data.Acid.Advanced  (query', update')
import Data.Acid.Local     (createCheckpointAndClose)
import Data.Data           (Data, Typeable)
import Data.Maybe          (isJust, fromJust)
import Data.Text           (pack)
import Data.Time
import Happstack.Server
import Happstack.Server.Compression

import AcidTypes
import DataTypes


myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)


initDB :: DB
initDB = DB [] [] [] []


main = do
   bracket (openLocalState initDB)
           (createCheckpointAndClose)
           (\acid -> do
               putStrLn $ "Server started at port 8000 - awaiting requests"
               simpleHTTP nullConf (handlers acid)
           )


handlers acid = do
   decodeBody myPolicy
   msum
      [ dir "v0.1" $ dir "get" $ doGet acid
      , dir "v0.1" $ dir "put" $ doPut acid
      , do
         liftIO $ putStrLn "*** unrecognized top-level handler"
         mzero
      ]


doGet acid = do
   liftIO $ putStrLn "Handling GET"
   msum
      [ dir "solarbodies" $ doGetSolarBodies acid
      , dir "feedtypes"   $ doGetFeedTypes acid
      , dir "stations"    $ doGetById "solarbodyid" (doGetStations acid)
      , dir "sensors"     $ doGetById "stationid" (doGetSensors acid)
      , do
         liftIO $ putStrLn "* unrecognized GET"
         mzero
      ]


doPut acid = do
   liftIO $ putStrLn "Handling POST"
   msum
      [ dir "solarbody" $ doPutSolarBody acid
      , dir "feedtype"  $ doPutFeedType acid
      , dir "station"   $ doPutStation acid
      , dir "sensor"    $ doPutSensor acid
      , do
         liftIO $ putStrLn "* unrecognized POST"
         mzero
      ]


doGetSolarBodies acid = do
   xs <- query' acid GetSolarBodies
   toJsonResponse ok xs


doGetFeedTypes acid = do
   xs <- query' acid GetFeedTypes
   toJsonResponse ok xs


doGetStations acid sbid = do
   let sbid' = readMaybe sbid :: Maybe Integer
   case sbid' of
      Nothing -> mzero
      Just i  -> do
         xs <- query' acid GetStations
         let xs' = filter (\x -> stationSolarBodyID x == i) xs
         toJsonResponse ok xs


doGetSensors acid stationId = do
   sensorIds <- optional $ queryString $ looks "sensorid"
   xs <- query' acid GetSensors
   let ids = map fromJust $ filter isJust $
         case sensorIds of
            Nothing   -> []
            Just sids -> map (\x -> readMaybe x :: Maybe Integer) sids
       xs' =
         case ids of
            [] -> xs
            _  -> filter (\x -> any (== sensorId x) ids) xs
   toJsonResponse ok xs'


doPutSolarBody acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String SolarBody
   case x of
      Left e -> do
         liftIO $ putStrLn $ "body didn't decode: " ++ e
         liftIO $ putStrLn $ B.unpack body
         mzero
      Right r -> do
         len <- update' acid (PutSolarBody r)
         toJsonResponse ok $ jsonCount len


doPutFeedType acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String FeedType
   case x of
      Left e -> do
         liftIO $ putStrLn $ "body didn't decode: " ++ e
         liftIO $ putStrLn $ B.unpack body
         mzero
      Right r -> do
         len <- update' acid (PutFeedType r)
         toJsonResponse ok $ jsonCount len


doPutStation acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String Station
   case x of
      Left e -> do
         liftIO $ putStrLn $ "body didn't decode: " ++ e
         liftIO $ putStrLn $ B.unpack body
         mzero
      Right r -> do
         len <- update' acid (PutStation r)
         toJsonResponse ok $ jsonCount len


doPutSensor acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String Sensor
   case x of
      Left e -> do
         liftIO $ putStrLn $ "body didn't decode: " ++ e
         liftIO $ putStrLn $ B.unpack body
         mzero
      Right r -> do
         len <- update' acid (PutSensor r)
         toJsonResponse ok $ jsonCount len


-----------------------------------------------------------
-- HELPERS
-----------------------------------------------------------


toJsonResponse functor msg = do
   compressedResponseFilter
   functor $ toResponse $ A.encode msg


doGetById i f = do
   msum
      [ do
         qid <- queryString $ look i
         f qid
      , do
         liftIO $ putStrLn $ "missing required var: " ++ i
         mzero
      ]


jsonCount = JsonCount . toInteger


readMaybe :: Read a => String -> Maybe a
readMaybe s =
   case reads s of
      [(x, "")] -> Just x
      _         -> Nothing


getBody :: ServerPart B.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return $ B.pack ""
