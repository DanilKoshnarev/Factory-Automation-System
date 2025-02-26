module Infrastructure.Config where

import Database.PostgreSQL.Simple

connectDb :: IO Connection
connectDb = connect defaultConnectInfo { connectDatabase = "factory_db" }
