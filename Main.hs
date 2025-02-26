module Main where

import App.Controllers.MachineController
import App.Controllers.SensorController
import Domain.Entities.Machine

main :: IO ()
main = do
    let machine = Machine { machineId = 1, machineName = "Conveyor Belt", status = "Operational" }
    createMachineHandler machine
    viewMachineHandler 1
    monitorSensorsHandler
