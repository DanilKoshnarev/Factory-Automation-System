module Domain.Repositories.SensorRepository where

import Domain.Entities.Sensor

class SensorRepository repo where
    saveSensor :: repo -> Sensor -> IO ()
    findSensorById :: repo -> Int -> IO (Maybe Sensor)
    findAllSensors :: repo -> IO [Sensor]
    deleteSensor :: repo -> Int -> IO ()
