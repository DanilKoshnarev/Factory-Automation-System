module Domain.Repositories.MachineRepository where

import Domain.Entities.Machine

class MachineRepository repo where
    saveMachine :: repo -> Machine -> IO ()
    findMachineById :: repo -> Int -> IO (Maybe Machine)
    findAllMachines :: repo -> IO [Machine]
    deleteMachine :: repo -> Int -> IO ()
