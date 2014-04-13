
module DataTypes
   ( SolarBody(..)
   , Feed(..)
   , Station(..)
   , Sensor(..)
   , Location(..)
   , LifeSupport(..)
   , Frequency(..)
   , Level(..)
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
      } deriving (Show,Read)


data Feed =
   Feed
      { feedId    :: Integer
      , feedType  :: Text
      , feedUnits :: Text
      } deriving (Show,Read)


data Location =
   Location
      { locationLatitude    :: Double
      , locationLongitude   :: Double
      , locationElevation   :: Double
      } deriving (Show,Read)


data Frequency =
   Frequency
      { frequencyLastUpdate :: Integer
      , frequencyInterval   :: Integer
      } deriving (Show,Read)


data Level =
   Level
      { levelFeedId  :: Integer
      , levelCurrent :: Double
      , levelMin     :: Maybe Double
      , levelMax     :: Maybe Double
      } deriving (Show,Read)


data LifeSupport =
   LifeSupport
      { lifeSupportIsHabitable :: Bool
      , lifeSupportLastChange  :: Maybe Integer
      } deriving (Show,Read)


data Sensor =
   Sensor
      { sensorId          :: Integer
      , sensorName        :: Text
      , sensorInfoUrl     :: Text
      , sensorLocation    :: Location
      , sensorFrequency   :: Frequency
      , sensorLifeSupport :: LifeSupport
      , sensorLevels      :: [Level]
      } deriving (Show,Read)


data Station =
   Station
      { stationID           :: Integer
      , stationName         :: Text
      , stationInfoUrl      :: Text
      , stationSolarBodyID  :: Integer
      , stationIsStationary :: Bool
      , stationSensors      :: [Sensor]
      } deriving (Show,Read)


-----------------------------------------------------------
-- INTERNAL Declarations
-----------------------------------------------------------


t_id           = pack "id"
t_name         = pack "name"
t_type         = pack "type"
t_units        = pack "units"
t_latitude     = pack "latitude"
t_longitude    = pack "longitude"
t_elevation    = pack "elevation"
t_lastUpdate   = pack "lastUpdate"
t_interval     = pack "interval"
t_feedID       = pack "feedID"
t_current      = pack "current"
t_min          = pack "min"
t_max          = pack "max"
t_isHabitable  = pack "isHabitable"
t_lastChange   = pack "lastChange"
t_infoUrl      = pack "infoUrl"
t_location     = pack "location"
t_frequency    = pack "frequency"
t_lifeSupport  = pack "lifeSupport"
t_levels       = pack "levels"
t_solarBodyID  = pack "solarBodyID"
t_isStationary = pack "isStationary"
t_sensors      = pack "sensors"


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


instance FromJSON Feed where
   parseJSON (Object x) =   Feed
                        <$> (x .: t_id)
                        <*> (x .: t_type)
                        <*> (x .: t_units)
   parseJSON _          = mzero


instance ToJSON Feed where
   toJSON (Feed a b c) = object
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
                        <*> (x .: t_name)
                        <*> (x .: t_infoUrl)
                        <*> (x .: t_location)
                        <*> (x .: t_frequency)
                        <*> (x .: t_lifeSupport)
                        <*> (x .: t_levels)
   parseJSON _          = mzero


instance ToJSON Sensor where
   toJSON (Sensor a b c d e f g) = object
      [ t_id          .= a
      , t_name        .= b
      , t_infoUrl     .= c
      , t_location    .= d
      , t_frequency   .= e
      , t_lifeSupport .= f
      , t_levels      .= g
      ]


instance FromJSON Station where
   parseJSON (Object x) =   Station
                        <$> (x .: t_id)
                        <*> (x .: t_name)
                        <*> (x .: t_infoUrl)
                        <*> (x .: t_solarBodyID)
                        <*> (x .: t_isStationary)
                        <*> (x .: t_sensors)
   parseJSON _          = mzero


instance ToJSON Station where
   toJSON (Station a b c d e f) = object
      [ t_id           .= a
      , t_name         .= b
      , t_infoUrl      .= c
      , t_solarBodyID  .= d
      , t_isStationary .= e
      , t_sensors      .= f
      ]
