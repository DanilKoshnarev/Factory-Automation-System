module App.Controllers.SensorController where

import Domain.UseCases.MonitorSensors
import Infrastructure.Persistence.Postgres.SensorRepositoryImpl
import Infrastructure.Config

monitorSensorsHandler :: IO ()
monitorSensorsHandler = do
    conn <- connectDb
    let repo = SensorRepositoryImpl conn
    sensors <- monitorSensors repo
    print sensors
