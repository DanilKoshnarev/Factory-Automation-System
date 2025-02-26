module Infrastructure.Persistence.Postgres.MachineRepositoryImpl where

import Domain.Entities.Machine
import Domain.Repositories.MachineRepository
import Database.PostgreSQL.Simple

data MachineRepositoryImpl = MachineRepositoryImpl Connection

instance MachineRepository MachineRepositoryImpl where
    saveMachine (MachineRepositoryImpl conn) machine = undefined 
    findMachineById (MachineRepositoryImpl conn) id = undefined 
    findAllMachines (MachineRepositoryImpl conn) = undefined 
    deleteMachine (MachineRepositoryImpl conn) id = undefined 
