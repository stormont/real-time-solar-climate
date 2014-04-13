{-# LANGUAGE DeriveDataTypeable,
             TemplateHaskell,
             TypeFamilies #-}
module AcidTypes
   where

import Control.Applicative  ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            (Query, Update, makeAcidic)
import Data.Data            (Data, Typeable)
import Data.List            (nub)
import Data.SafeCopy        (base, deriveSafeCopy)

import DataTypes


data DB =
   DB
      { dbSolarBodies :: [SolarBody]
      , dbFeedTypes   :: [FeedType]
      , dbStations    :: [Station]
      , dbSensors     :: [Sensor]
      } deriving (Show,Read,Data,Typeable)


class FromDB a where
   fromDB :: DB -> [a]


class ToDB a where
   toDB :: DB -> [a] -> DB


instance FromDB SolarBody where
   fromDB = dbSolarBodies


instance ToDB SolarBody where
   toDB db xs = db { dbSolarBodies = xs }


instance FromDB FeedType where
   fromDB = dbFeedTypes


instance ToDB FeedType where
   toDB db xs = db { dbFeedTypes = xs }


instance FromDB Station where
   fromDB = dbStations


instance ToDB Station where
   toDB db xs = db { dbStations = xs }


instance FromDB Sensor where
   fromDB = dbSensors


instance ToDB Sensor where
   toDB db xs = db { dbSensors = xs }


getSolarBodies :: Query DB [SolarBody]
getSolarBodies = dbSolarBodies <$> ask


getFeedTypes :: Query DB [FeedType]
getFeedTypes = dbFeedTypes <$> ask


getStations :: Query DB [Station]
getStations = dbStations <$> ask


getSensors :: Query DB [Sensor]
getSensors = dbSensors <$> ask


putSolarBody :: SolarBody -> Update DB Integer
putSolarBody x = do
   db <- get
   let xs = fromDB db
       m = if solarBodyId x == 0 && xs /= []
            then (+ 1) $ maximum $ map solarBodyId xs
            else solarBodyId x
       ys = nub
          $ (:) (x { solarBodyId = m })
          $ filter (\y -> solarBodyId y /= solarBodyId x) xs
   put $ toDB db ys
   return m


putFeedType :: FeedType -> Update DB Integer
putFeedType x = do
   db <- get
   let xs = fromDB db
       m = if feedTypeId x == 0 && xs /= []
            then (+ 1) $ maximum $ map feedTypeId xs
            else feedTypeId x
       ys = nub
          $ (:) (x { feedTypeId = m })
          $ filter (\y -> feedTypeId y /= feedTypeId x) xs
   put $ toDB db ys
   return m


putStation :: Station -> Update DB Integer
putStation x = do
   db <- get
   let xs = fromDB db
       m = if stationId x == 0 && xs /= []
            then (+ 1) $ maximum $ map stationId xs
            else stationId x
       ys = nub
          $ (:) (x { stationId = m })
          $ filter (\y -> stationId y /= stationId x) xs
   put $ toDB db ys
   return m


putSensor :: Sensor -> Update DB Integer
putSensor x = do
   db <- get
   let xs = fromDB db
       m = if sensorId x == 0 && xs /= []
            then (+ 1) $ maximum $ map sensorId xs
            else sensorId x
       ys = nub
          $ (:) (x { sensorId = m })
          $ filter (\y -> sensorId y /= sensorId x) xs
   put $ toDB db ys
   return m


deleteSolarBody :: Integer -> Update DB Int
deleteSolarBody x = do
   db <- get
   let xs = fromDB db
       ys = filter (\y -> solarBodyId y /= x) xs
   put $ toDB db ys
   return $ length ys


deleteFeedType :: Integer -> Update DB Int
deleteFeedType x = do
   db <- get
   let xs = fromDB db
       ys = filter (\y -> feedTypeId y /= x) xs
   put $ toDB db ys
   return $ length ys


deleteStation :: Integer -> Update DB Int
deleteStation x = do
   db <- get
   let xs = fromDB db
       ys = filter (\y -> stationId y /= x) xs
   put $ toDB db ys
   return $ length ys


deleteSensor :: Integer -> Update DB Int
deleteSensor x = do
   db <- get
   let xs = fromDB db
       ys = filter (\y -> sensorId y /= x) xs
   put $ toDB db ys
   return $ length ys


$(deriveSafeCopy 0 'base ''SolarBody)
$(deriveSafeCopy 0 'base ''FeedType)
$(deriveSafeCopy 0 'base ''Station)
$(deriveSafeCopy 0 'base ''LifeSupport)
$(deriveSafeCopy 0 'base ''Frequency)
$(deriveSafeCopy 0 'base ''Location)
$(deriveSafeCopy 0 'base ''Level)
$(deriveSafeCopy 0 'base ''Sensor)
$(deriveSafeCopy 0 'base ''DB)


$(makeAcidic ''DB [ 'getSolarBodies
                  , 'getFeedTypes
                  , 'getStations
                  , 'getSensors
                  , 'putSolarBody
                  , 'putFeedType
                  , 'putStation
                  , 'putSensor
                  , 'deleteSolarBody
                  , 'deleteFeedType
                  , 'deleteStation
                  , 'deleteSensor
                  ])
