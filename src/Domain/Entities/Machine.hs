module Domain.Entities.Machine where

data Machine = Machine
    { machineId :: Int
    , machineName :: String
    , status :: String
    } deriving (Show, Eq)
