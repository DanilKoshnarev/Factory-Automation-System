module Domain.UseCases.MonitorSensors where

import Domain.Entities.Sensor
import Domain.Repositories.SensorRepository

monitorSensors :: (SensorRepository repo) => repo -> IO [Sensor]
monitorSensors repo = findAllSensors repo
