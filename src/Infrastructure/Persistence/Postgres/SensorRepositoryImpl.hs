module Infrastructure.Persistence.Postgres.SensorRepositoryImpl where

import Domain.Entities.Sensor
import Domain.Repositories.SensorRepository
import Database.PostgreSQL.Simple

data SensorRepositoryImpl = SensorRepositoryImpl Connection

instance SensorRepository SensorRepositoryImpl where
    saveSensor (SensorRepositoryImpl conn) sensor = undefined 
    findSensorById (SensorRepositoryImpl conn) id = undefined 
    findAllSensors (SensorRepositoryImpl conn) = undefined 
    deleteSensor (SensorRepositoryImpl conn) id = undefined 
