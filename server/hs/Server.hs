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
import System.Random
import Prelude hiding (log)

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
      [ do
         rq <- askRq
         liftIO $ log rq
         mzero
      , dir "v0.1" $ dir "get"    $ doGet acid
      , dir "v0.1" $ dir "put"    $ doPut acid
      , dir "v0.1" $ dir "delete" $ doDelete acid
      , do
         liftIO $ putStrLn "HTTP 404"
         toJsonResponse notFound $ HttpResponse 404 $ pack "Resource not found"
      ]


doGet acid = do
   rq <- askRq
   let meth = rqMethod rq
   if meth /= GET && meth /= HEAD
      then mzero
      else do
         liftIO $ putStrLn "Handling GET"
         msum
            [ dir "solarbodies" $ doGetSolarBodies acid
            , dir "feedtypes"   $ doGetFeedTypes acid
            , dir "stations"    $ doGetById "solarbodyid" (doGetStations acid)
            , dir "sensors"     $ doGetById "stationid" (doGetSensors acid)
            , dir "random"      $ doGetRandom acid
            , do
               liftIO $ putStrLn "HTTP 400 - unrecognized GET"
               toJsonResponse badRequest $ HttpResponse 400 $ pack "Invalid get/ request"
            ]


doPut acid = do
   rq <- askRq
   let meth = rqMethod rq
   if meth /= POST && meth /= OPTIONS
      then mzero
      else do
         liftIO $ putStrLn "Handling POST"
         msum
            [ dir "solarbody" $ doPutSolarBody acid
            , dir "feedtype"  $ doPutFeedType acid
            , dir "station"   $ doPutStation acid
            , dir "sensor"    $ doPutSensor acid
            , do
               liftIO $ putStrLn "HTTP 400 - unrecognized GET"
               toJsonResponse badRequest $ HttpResponse 400 $ pack "Invalid post/ request"
            ]


doDelete acid = do
   rq <- askRq
   let meth = rqMethod rq
   if meth /= POST
      then mzero
      else do
         liftIO $ putStrLn "Handling DELETE"
         msum
            [ dir "solarbody" $ doDeleteSolarBody acid
            , dir "feedtype"  $ doDeleteFeedType acid
            , dir "station"   $ doDeleteStation acid
            , dir "sensor"    $ doDeleteSensor acid
            , do
               liftIO $ putStrLn "HTTP 400 - unrecognized DELETE"
               toJsonResponse badRequest $ HttpResponse 400 $ pack "Invalid delete/ request"
            ]


doGetSolarBodies acid = do
   xs <- query' acid GetSolarBodies
   toJsonResponse ok xs


doGetFeedTypes acid = do
   xs <- query' acid GetFeedTypes
   toJsonResponse ok xs


doGetStations acid parentId = do
   let parentId' = readMaybe parentId :: Maybe Integer
   case parentId' of
      Nothing -> do
         let msg = "solarbodyid is not an integer"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ parentId
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Just i  -> do
         xs <- query' acid GetStations
         let xs' = filter (\x -> stationSolarBodyId x == i) xs
         toJsonResponse ok xs'


doGetSensors acid parentId = do
   let parentId' = readMaybe parentId :: Maybe Integer
   case parentId' of
      Nothing -> do
         let msg = "stationid is not an integer"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ parentId
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Just i  -> do
         sensorIds <- optional $ queryString $ looks "sensorid"
         xs <- query' acid GetSensors
         let xs' = filter (\x -> sensorStationId x == i) xs
             ids = map fromJust $ filter isJust $
               case sensorIds of
                  Nothing   -> []
                  Just sids -> map (\x -> readMaybe x :: Maybe Integer) sids
             xs'' =
               case ids of
                  [] -> xs'
                  _  -> filter (\x -> any (== sensorId x) ids) xs'
         toJsonResponse ok xs''


getRandIndex :: [a] -> IO a
getRandIndex xs = do
   let n = length xs - 1
   i <- getStdRandom (randomR (0,n))
   return $ xs !! i


doGetRandom acid = do
   solarBodies <- query' acid GetSolarBodies
   if solarBodies == []
      then do
         liftIO $ putStrLn "solarBodies is empty"
         mzero
      else do
         solarBody <- liftIO $ getRandIndex solarBodies
         stations' <- query' acid GetStations
         let stations = filter (\x -> stationSolarBodyId x == solarBodyId solarBody) stations'
         if stations == []
            then do
               liftIO $ putStrLn "stations is empty"
               mzero
            else do
               station <- liftIO $ getRandIndex stations
               sensors' <- query' acid GetSensors
               let sensors = filter (\x -> sensorStationId x == stationId station) sensors'
               if sensors == []
                  then do
                     liftIO $ putStrLn "sensors is empty"
                     mzero
                  else do
                     sensor <- liftIO $ getRandIndex sensors
                     feedTypes' <- query' acid GetFeedTypes
                     let level = head $ sensorLevels sensor
                     let feedType = head $ filter (\x -> feedTypeId x == levelFeedTypeId level) feedTypes'
                     toJsonResponse ok $
                        RandomSensor
                           (solarBodyId solarBody)
                           (solarBodyName solarBody)
                           (stationId station)
                           (stationName station)
                           (sensorId sensor)
                           (sensorName sensor)
                           (feedTypeName feedType)
                           (levelCurrent level)
                           (feedTypeUnits feedType)


doPutSolarBody acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String SolarBody
   case x of
      Left e -> do
         let msg = "Invalid solarbody JSON"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ e ++ " - " ++ (B.unpack body)
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Right r -> do
         len <- update' acid (PutSolarBody r)
         toJsonResponse ok $ toJsonId len


doPutFeedType acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String FeedType
   case x of
      Left e -> do
         let msg = "Invalid feedtype JSON"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ e
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Right r -> do
         len <- update' acid (PutFeedType r)
         toJsonResponse ok $ toJsonId len


doPutStation acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String Station
   case x of
      Left e -> do
         let msg = "Invalid station JSON"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ e
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Right r -> do
         len <- update' acid (PutStation r)
         toJsonResponse ok $ toJsonId len


doPutSensor acid = do
   body <- getBody
   let x = A.eitherDecode body :: Either String Sensor
   case x of
      Left e -> do
         let msg = "Invalid sensor JSON"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ e
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Right r -> do
         len <- update' acid (PutSensor r)
         toJsonResponse ok $ toJsonId len


doDeleteSolarBody acid = do
   ident <- queryString $ look "id"
   let sbid = readMaybe ident :: Maybe Integer
   case sbid of
      Nothing -> do
         let msg = "id is not an integer"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ ident
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Just r -> do
         len <- update' acid (DeleteSolarBody r)
         toJsonResponse ok $ toJsonCount len


doDeleteFeedType acid = do
   ident <- queryString $ look "id"
   let i = readMaybe ident :: Maybe Integer
   case i of
      Nothing -> do
         let msg = "id is not an integer"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ ident
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Just r -> do
         len <- update' acid (DeleteFeedType r)
         toJsonResponse ok $ toJsonCount len


doDeleteStation acid = do
   ident <- queryString $ look "id"
   let i = readMaybe ident :: Maybe Integer
   case i of
      Nothing -> do
         let msg = "id is not an integer"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ ident
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Just r -> do
         len <- update' acid (DeleteStation r)
         toJsonResponse ok $ toJsonCount len


doDeleteSensor acid = do
   ident <- queryString $ look "id"
   let i = readMaybe ident :: Maybe Integer
   case i of
      Nothing -> do
         let msg = "id is not an integer"
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg ++ ": " ++ ident
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      Just r -> do
         len <- update' acid (DeleteSensor r)
         toJsonResponse ok $ toJsonCount len


-----------------------------------------------------------
-- HELPERS
-----------------------------------------------------------


log rq = do
   t <- getCurrentTime
   let (host,port) = rqPeer rq
       meth = rqMethod rq
   liftIO $ putStrLn $  (show t)
                     ++ " - " ++ host ++ ":" ++ (show port)
                     ++ " - " ++ (show meth)
                     ++ " - " ++ (rqUri rq)


toJsonResponse functor msg = do
   compressedResponseFilter
   addHeaderM "Access-Control-Allow-Origin" "*"
   functor $ toResponse $ A.encode msg


doGetById i f = do
   msum
      [ do
         qid <- queryString $ look i
         f qid
      , do
         let msg = "missing required var: " ++ i
         liftIO $ putStrLn $ "HTTP 400 - " ++ msg
         toJsonResponse badRequest $ HttpResponse 400 $ pack msg
      ]


toJsonCount = JsonCount . toInteger


toJsonId = JsonID . toInteger


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
