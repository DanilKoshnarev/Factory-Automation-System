module App.Controllers.MachineController where

import Domain.UseCases.ManageMachines
import Infrastructure.Persistence.Postgres.MachineRepositoryImpl
import Infrastructure.Config
import Domain.Entities.Machine

createMachineHandler :: Machine -> IO ()
createMachineHandler machine = do
    conn <- connectDb
    let repo = MachineRepositoryImpl conn
    createMachine repo machine

viewMachineHandler :: Int -> IO ()
viewMachineHandler machineId = do
    conn <- connectDb
    let repo = MachineRepositoryImpl conn
    machine <- viewMachine repo machineId
    print machine
