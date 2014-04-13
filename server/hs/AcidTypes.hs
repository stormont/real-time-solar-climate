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


putSolarBody :: SolarBody -> Update DB Int
putSolarBody x = do
   db <- get
   let xs = fromDB db
       ys = nub $ (x : xs)
   put $ toDB db ys
   return $ length ys


putFeedType :: FeedType -> Update DB Int
putFeedType x = do
   db <- get
   let xs = fromDB db
       ys = nub $ (x : xs)
   put $ toDB db ys
   return $ length ys


putStation :: Station -> Update DB Int
putStation x = do
   db <- get
   let xs = fromDB db
       ys = nub $ (x : xs)
   put $ toDB db ys
   return $ length ys


putSensor :: Sensor -> Update DB Int
putSensor x = do
   db <- get
   let xs = fromDB db
       ys = nub $ (x : xs)
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
                  ])
