module Domain.UseCases.ManageMachines where

import Domain.Entities.Machine
import Domain.Repositories.MachineRepository

createMachine :: (MachineRepository repo) => repo -> Machine -> IO ()
createMachine repo machine = saveMachine repo machine

viewMachine :: (MachineRepository repo) => repo -> Int -> IO (Maybe Machine)
viewMachine repo machineId = findMachineById repo machineId

viewAllMachines :: (MachineRepository repo) => repo -> IO [Machine]
viewAllMachines repo = findAllMachines repo

removeMachine :: (MachineRepository repo) => repo -> Int -> IO ()
removeMachine repo machineId = deleteMachine repo machineId
