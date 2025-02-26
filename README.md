# Factory Automation System

## Опис
Factory Automation System - це система для автоматизації механізмів на фабриці, побудована на Haskell з використанням принципів Domain-Driven Design (DDD). Система включає модулі для управління машинами і сенсорами, а також моніторингу сенсорів.

## Структура проекту
Проект розділений на кілька шарів для покращення читабельності та підтримуваності коду:

- **Domain**: Основна бізнес-логіка та правила.
- **Application**: Інтерфейси, юзкейси та реалізації для роботи з даними.
- **Infrastructure**: Реалізація деталей інфраструктури, таких як репозиторії даних.
- **Presentation**: Взаємодія з користувачем.

## Встановлення
1. Клонуйте репозиторій:
    ```bash
    git clone <URL репозиторію>
    ```
2. Перейдіть до каталогу проекту:
    ```bash
    cd factory-automation
    ```
3. Встановіть необхідні залежності:
    ```bash
    stack setup
    stack build
    ```

## Запуск
Для запуску проекту виконайте команду:
```bash
stack run
```

## Структура каталогів
```plaintext
factory-automation/
├── src/
│   ├── Domain/
│   │   ├── Entities/
│   │   │   ├── Machine.hs
│   │   │   ├── Sensor.hs
│   │   ├── Repositories/
│   │   │   ├── MachineRepository.hs
│   │   │   └── SensorRepository.hs
│   │   ├── Services/
│   │   │   ├── MachineService.hs
│   │   │   └── SensorService.hs
│   │   └── UseCases/
│   │       ├── ManageMachines.hs
│   │       └── MonitorSensors.hs
├── Infrastructure/
│   ├── Persistence/
│   │   ├── Postgres/
│   │   │   ├── MachineRepositoryImpl.hs
│   │   │   └── SensorRepositoryImpl.hs
│   └── Config.hs
├── App/
│   ├── Controllers/
│   │   └── MachineController.hs
│   │   └── SensorController.hs
├── Main.hs
├── Setup.hs
├── stack.yaml
└── README.md
```

### Опис компонентів

#### Domain
- **Machine.hs**: Клас сутності машини.
    ```haskell
    module Domain.Entities.Machine where

    data Machine = Machine
        { machineId :: Int
        , machineName :: String
        , status :: String
        } deriving (Show, Eq)
    ```

- **Sensor.hs**: Клас сутності сенсора.
    ```haskell
    module Domain.Entities.Sensor where

    data Sensor = Sensor
        { sensorId :: Int
        , sensorType :: String
        , value :: Double
        } deriving (Show, Eq)
    ```

- **MachineRepository.hs**: Інтерфейс репозиторію машин.
    ```haskell
    module Domain.Repositories.MachineRepository where

    import Domain.Entities.Machine

    class MachineRepository repo where
        saveMachine :: repo -> Machine -> IO ()
        findMachineById :: repo -> Int -> IO (Maybe Machine)
        findAllMachines :: repo -> IO [Machine]
        deleteMachine :: repo -> Int -> IO ()
    ```

- **SensorRepository.hs**: Інтерфейс репозиторію сенсорів.
    ```haskell
    module Domain.Repositories.SensorRepository where

    import Domain.Entities.Sensor

    class SensorRepository repo where
        saveSensor :: repo -> Sensor -> IO ()
        findSensorById :: repo -> Int -> IO (Maybe Sensor)
        findAllSensors :: repo -> IO [Sensor]
        deleteSensor :: repo -> Int -> IO ()
    ```

#### Application
- **ManageMachines.hs**: Юзкейс для управління машинами.
    ```haskell
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
    ```

- **MonitorSensors.hs**: Юзкейс для моніторингу сенсорів.
    ```haskell
    module Domain.UseCases.MonitorSensors where

    import Domain.Entities.Sensor
    import Domain.Repositories.SensorRepository

    monitorSensors :: (SensorRepository repo) => repo -> IO [Sensor]
    monitorSensors repo = findAllSensors repo
    ```

#### Infrastructure
- **MachineRepositoryImpl.hs**: Реалізація репозиторію машин з використанням PostgreSQL.
    ```haskell
    module Infrastructure.Persistence.Postgres.MachineRepositoryImpl where

    import Domain.Entities.Machine
    import Domain.Repositories.MachineRepository
    import Database.PostgreSQL.Simple

    data MachineRepositoryImpl = MachineRepositoryImpl Connection

    instance MachineRepository MachineRepositoryImpl where
        saveMachine (MachineRepositoryImpl conn) machine = undefined -- Реалізація збереження машини в базу даних
        findMachineById (MachineRepositoryImpl conn) id = undefined -- Реалізація пошуку машини за ID
        findAllMachines (MachineRepositoryImpl conn) = undefined -- Реалізація отримання всіх машин
        deleteMachine (MachineRepositoryImpl conn) id = undefined -- Реалізація видалення машини
    ```

- **SensorRepositoryImpl.hs**: Реалізація репозиторію сенсорів з використанням PostgreSQL.
    ```haskell
    module Infrastructure.Persistence.Postgres.SensorRepositoryImpl where

    import Domain.Entities.Sensor
    import Domain.Repositories.SensorRepository
    import Database.PostgreSQL.Simple

    data SensorRepositoryImpl = SensorRepositoryImpl Connection

    instance SensorRepository SensorRepositoryImpl where
        saveSensor (SensorRepositoryImpl conn) sensor = undefined -- Реалізація збереження сенсора в базу даних
        findSensorById (SensorRepositoryImpl conn) id = undefined -- Реалізація пошуку сенсора за ID
        findAllSensors (SensorRepositoryImpl conn) = undefined -- Реалізація отримання всіх сенсорів
        deleteSensor (SensorRepositoryImpl conn) id = undefined -- Реалізація видалення сенсора
    ```

- **Config.hs**: Конфігурація підключення до бази даних.
    ```haskell
    module Infrastructure.Config where

    import Database.PostgreSQL.Simple

    connectDb :: IO Connection
    connectDb = connect defaultConnectInfo { connectDatabase = "factory_db" }
    ```

#### Presentation
- **MachineController.hs**: Контролер для управління машинами.
    ```haskell
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
    ```

- **SensorController.hs**: Контролер для моніторингу сенсорів.
    ```haskell
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
    ```

### Main.hs

```haskell
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
```

### stack.yaml

```yaml
resolver: lts-16.27

packages:
- .

extra-deps:
- postgresql-simple-0.6.4
- reactive-banana-1.2.1.0

flags: {}

extra-package-dbs: []
```

### Setup.hs

```haskell
import Distribution.Simple
main = defaultMain
```

### Ліцензія
Цей проект ліцензовано під ліцензією MIT. 
