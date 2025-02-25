module Domain.Entities.Sensor where

data Sensor = Sensor
    { sensorId :: Int
    , sensorType :: String
    , value :: Double
    } deriving (Show, Eq)
