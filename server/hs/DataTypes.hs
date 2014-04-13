{-# LANGUAGE DeriveDataTypeable #-}
module DataTypes
   ( SolarBody(..)
   , FeedType(..)
   , Station(..)
   , Sensor(..)
   , Location(..)
   , LifeSupport(..)
   , Frequency(..)
   , Level(..)
   , JsonCount(..)
   , JsonID(..)
   , HttpResponse(..)
   , RandomSensor(..)
   ) where

import Control.Applicative ( (<*>), (<$>) )
import Control.Monad ( mzero )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..), (.:), (.:?), (.=), object )
import Data.Data ( Data, Typeable )
import Data.Text ( Text(..), pack )


-----------------------------------------------------------
-- EXTERNAL Declarations
-----------------------------------------------------------


data SolarBody =
   SolarBody
      { solarBodyId   :: Integer
      , solarBodyName :: Text
      } deriving (Show,Read,Data,Typeable)


instance Eq SolarBody where
   (==) lhs rhs = solarBodyId lhs == solarBodyId rhs


data FeedType =
   FeedType
      { feedTypeId    :: Integer
      , feedTypeName  :: Text
      , feedTypeUnits :: Text
      } deriving (Show,Read,Data,Typeable)


instance Eq FeedType where
   (==) lhs rhs = feedTypeId lhs == feedTypeId rhs


data Location =
   Location
      { locationLatitude    :: Double
      , locationLongitude   :: Double
      , locationElevation   :: Double
      } deriving (Show,Read,Data,Typeable,Eq)


data Frequency =
   Frequency
      { frequencyLastUpdate :: Integer
      , frequencyInterval   :: Integer
      } deriving (Show,Read,Data,Typeable,Eq)


data Level =
   Level
      { levelFeedTypeId  :: Integer
      , levelCurrent     :: Double
      , levelMin         :: Maybe Double
      , levelMax         :: Maybe Double
      } deriving (Show,Read,Data,Typeable,Eq)


data LifeSupport =
   LifeSupport
      { lifeSupportIsHabitable :: Bool
      , lifeSupportLastChange  :: Maybe Integer
      } deriving (Show,Read,Data,Typeable,Eq)


data Sensor =
   Sensor
      { sensorId          :: Integer
      , sensorStationId   :: Integer
      , sensorName        :: Text
      , sensorInfoUrl     :: Text
      , sensorLocation    :: Location
      , sensorFrequency   :: Frequency
      , sensorLifeSupport :: LifeSupport
      , sensorLevels      :: [Level]
      } deriving (Show,Read,Data,Typeable)


instance Eq Sensor where
   (==) lhs rhs = sensorId lhs == sensorId rhs


data Station =
   Station
      { stationId           :: Integer
      , stationName         :: Text
      , stationInfoUrl      :: Text
      , stationSolarBodyId  :: Integer
      , stationIsStationary :: Bool
      } deriving (Show,Read,Data,Typeable)


instance Eq Station where
   (==) lhs rhs = stationId lhs == stationId rhs


data JsonCount =
   JsonCount
      { jcCount :: Integer
      } deriving (Show,Read)


data JsonID =
   JsonID
      { jsonId :: Integer
      } deriving (Show,Read)


data HttpResponse =
   HttpResponse
      { httpResponseCode    :: Int
      , httpResponseMessage :: Text
      } deriving (Show,Read)


data RandomSensor =
   RandomSensor
      { randomSensorSolarBodyId   :: Integer
      , randomSensorSolarBodyName :: Text
      , randomSensorStationId     :: Integer
      , randomSensorStationName   :: Text
      , randomSensorSensorId      :: Integer
      , randomSensorSensorName    :: Text
      , randomSensorFeedType      :: Text
      , randomSensorLevelCurrent  :: Double
      , randomSensorFeedTypeUnits :: Text
      } deriving (Show,Read)


-----------------------------------------------------------
-- INTERNAL Declarations
-----------------------------------------------------------


t_id            = pack "id"
t_name          = pack "name"
t_type          = pack "type"
t_units         = pack "units"
t_latitude      = pack "latitude"
t_longitude     = pack "longitude"
t_elevation     = pack "elevation"
t_lastUpdate    = pack "lastUpdate"
t_interval      = pack "interval"
t_feedID        = pack "feedTypeID"
t_current       = pack "current"
t_min           = pack "min"
t_max           = pack "max"
t_isHabitable   = pack "isHabitable"
t_lastChange    = pack "lastChange"
t_infoUrl       = pack "infoUrl"
t_location      = pack "location"
t_frequency     = pack "frequency"
t_lifeSupport   = pack "lifeSupport"
t_levels        = pack "levels"
t_solarBodyID   = pack "solarBodyID"
t_isStationary  = pack "isStationary"
t_count         = pack "count"
t_stationId     = pack "stationID"
t_code          = pack "code"
t_message       = pack "message"
t_solarBodyId   = pack "solarBodyID"
t_solarBodyName = pack "solarBodyName"
t_stationName   = pack "stationName"
t_sensorId      = pack "sensorID"
t_sensorName    = pack "sensorName"
t_currentLevel  = pack "currentLevel"


instance FromJSON SolarBody where
   parseJSON (Object x) =   SolarBody
                        <$> (x .: t_id)
                        <*> (x .: t_name)
   parseJSON _          = mzero


instance ToJSON SolarBody where
   toJSON (SolarBody a b) = object
      [ t_id   .= a
      , t_name .= b
      ]


instance FromJSON FeedType where
   parseJSON (Object x) =   FeedType
                        <$> (x .: t_id)
                        <*> (x .: t_type)
                        <*> (x .: t_units)
   parseJSON _          = mzero


instance ToJSON FeedType where
   toJSON (FeedType a b c) = object
      [ t_id    .= a
      , t_type  .= b
      , t_units .= c
      ]


instance FromJSON Location where
   parseJSON (Object x) =   Location
                        <$> (x .: t_latitude)
                        <*> (x .: t_longitude)
                        <*> (x .: t_elevation)
   parseJSON _          = mzero


instance ToJSON Location where
   toJSON (Location a b c) = object
      [ t_latitude  .= a
      , t_longitude .= b
      , t_elevation .= c
      ]


instance FromJSON Frequency where
   parseJSON (Object x) =   Frequency
                        <$> (x .: t_lastUpdate)
                        <*> (x .: t_interval)
   parseJSON _          = mzero


instance ToJSON Frequency where
   toJSON (Frequency a b) = object
      [ t_lastUpdate .= a
      , t_interval   .= b
      ]


instance FromJSON Level where
   parseJSON (Object x) =   Level
                        <$> (x .:  t_feedID)
                        <*> (x .:  t_current)
                        <*> (x .:? t_min)
                        <*> (x .:? t_max)
   parseJSON _          = mzero


instance ToJSON Level where
   toJSON (Level a b c d) = object
      [ t_feedID  .= a
      , t_current .= b
      , t_min     .= c
      , t_max     .= d
      ]


instance FromJSON LifeSupport where
   parseJSON (Object x) =   LifeSupport
                        <$> (x .:  t_isHabitable)
                        <*> (x .:? t_lastChange)
   parseJSON _          = mzero


instance ToJSON LifeSupport where
   toJSON (LifeSupport a b) = object
      [ t_isHabitable .= a
      , t_lastChange  .= b
      ]


instance FromJSON Sensor where
   parseJSON (Object x) =   Sensor
                        <$> (x .: t_id)
                        <*> (x .: t_stationId)
                        <*> (x .: t_name)
                        <*> (x .: t_infoUrl)
                        <*> (x .: t_location)
                        <*> (x .: t_frequency)
                        <*> (x .: t_lifeSupport)
                        <*> (x .: t_levels)
   parseJSON _          = mzero


instance ToJSON Sensor where
   toJSON (Sensor a b c d e f g h) = object
      [ t_id          .= a
      , t_stationId   .= b
      , t_name        .= c
      , t_infoUrl     .= d
      , t_location    .= e
      , t_frequency   .= f
      , t_lifeSupport .= g
      , t_levels      .= h
      ]


instance FromJSON Station where
   parseJSON (Object x) =   Station
                        <$> (x .: t_id)
                        <*> (x .: t_name)
                        <*> (x .: t_infoUrl)
                        <*> (x .: t_solarBodyID)
                        <*> (x .: t_isStationary)
   parseJSON _          = mzero


instance ToJSON Station where
   toJSON (Station a b c d e) = object
      [ t_id           .= a
      , t_name         .= b
      , t_infoUrl      .= c
      , t_solarBodyID  .= d
      , t_isStationary .= e
      ]


instance FromJSON JsonCount where
   parseJSON (Object x) =   JsonCount
                        <$> (x .: t_count)
   parseJSON _          = mzero


instance ToJSON JsonCount where
   toJSON (JsonCount a) = object
      [ t_count .= a
      ]


instance FromJSON JsonID where
   parseJSON (Object x) =   JsonID
                        <$> (x .: t_id)
   parseJSON _          = mzero


instance ToJSON JsonID where
   toJSON (JsonID a) = object
      [ t_id .= a
      ]


instance FromJSON HttpResponse where
   parseJSON (Object x) =   HttpResponse
                        <$> (x .: t_code)
                        <*> (x .: t_message)
   parseJSON _          = mzero


instance ToJSON HttpResponse where
   toJSON (HttpResponse a b) = object
      [ t_code    .= a
      , t_message .= b
      ]


instance FromJSON RandomSensor where
   parseJSON (Object x) =   RandomSensor
                        <$> (x .: t_solarBodyId)
                        <*> (x .: t_solarBodyName)
                        <*> (x .: t_stationId)
                        <*> (x .: t_stationName)
                        <*> (x .: t_sensorId)
                        <*> (x .: t_sensorName)
                        <*> (x .: t_type)
                        <*> (x .: t_currentLevel)
                        <*> (x .: t_units)
   parseJSON _          = mzero


instance ToJSON RandomSensor where
   toJSON (RandomSensor a b c d e f g h i) = object
      [ t_solarBodyId   .= a
      , t_solarBodyName .= b
      , t_stationId     .= c
      , t_stationName   .= d
      , t_sensorId      .= e
      , t_sensorName    .= f
      , t_type          .= g
      , t_currentLevel  .= h
      , t_units         .= i
      ]
