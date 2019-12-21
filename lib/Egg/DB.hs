{-# LANGUAGE OverloadedStrings #-}

module Egg.DB where

import qualified Data.ByteString.Char8 as BS8
import Data.Function ((&))
import Data.String
import qualified Data.String as String
import qualified Database.PostgreSQL.Simple as SQL
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Envy as Envy

tableCreateString :: (IsString a) => a
tableCreateString = "CREATE TABLE IF NOT EXISTS events (ID serial NOT NULL PRIMARY KEY, info json NOT NULL);"

createSchema :: SQL.Connection -> IO ()
createSchema connection =
  SQL.execute_ connection tableCreateString
    >> pure ()

data Config
  = Config
      { configDatabase :: BS8.ByteString,
        configHost :: Warp.HostPreference,
        configPort :: Warp.Port
      }
  deriving (Eq, Show)

instance Envy.FromEnv Config where
  fromEnv = do
    database <- Envy.env "DATABASE"
    host <- Envy.env "HOST"
    port <- Envy.env "PORT"
    pure Config
      { configDatabase = database,
        configHost = String.fromString host,
        configPort = read port
      }

makeSettings :: Config -> Warp.Settings
makeSettings config =
  Warp.defaultSettings & Warp.setHost (configHost config)
    & Warp.setPort (configPort config)
