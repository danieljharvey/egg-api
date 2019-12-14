{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class
-- import Egg.EventTypes

import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Function              ((&))
import qualified Data.Map                   as Map
import           Data.Monoid                ((<>))
import           Data.String
import qualified Data.String                as String
import qualified Database.PostgreSQL.Simple as SQL
import qualified Egg.API                    as API
import qualified Egg.EventStore             as EventStore
import qualified Egg.SampleProjections      as Sample
import qualified Network.HTTP.Types         as HTTP
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.Envy                as Envy

type EventRow =
  (Int, JSON.Value)

tableCreateString :: (IsString a) => a
tableCreateString = "CREATE TABLE IF NOT EXISTS events (ID serial NOT NULL PRIMARY KEY, info json NOT NULL);"

main :: IO ()
main = do
  config' <- Envy.decode
  case config' of
    Nothing -> putStrLn "Could not read env vars"
    Just config -> do
      print config
      let settings = makeSettings config
      connection <- SQL.connectPostgreSQL (configDatabase config)
      createSchema connection
      projections' <- EventStore.createMVar Sample.eggBoardProjection
      Warp.runSettings settings (application API.sampleAPI projections' connection)

createSchema :: SQL.Connection -> IO ()
createSchema connection =
  SQL.execute_ connection tableCreateString
    >> pure ()

data Config
  = Config
      { configDatabase :: BS8.ByteString,
        configHost     :: Warp.HostPreference,
        configPort     :: Warp.Port
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

writeEvent :: SQL.Connection -> Wai.Request -> IO [EventRow]
writeEvent connection request = do
  json <- liftIO (Wai.requestBody request)
  SQL.withTransaction
    connection
    ( do
        _ <-
          SQL.execute
            connection
            "INSERT INTO events (info) VALUES (?);"
            [json]
        allEvents <-
          SQL.query_
            connection
            "SELECT * FROM events"
        pure allEvents
    )

eventListToMap :: [EventRow] -> EventStore.EventList
eventListToMap = Map.fromList

application ::
  (JSON.FromJSON action, Show state) =>
  API.API state ->
  EventStore.StatefulProjection action state ->
  SQL.Connection ->
  Wai.Application
application api projection connection request respond =
  if Wai.requestMethod request == HTTP.methodPost
  then do
      -- post means plop an event in the store
      allEvents' <- writeEvent connection request
      plops <- EventStore.runStatefulProjection projection (eventListToMap allEvents')
      let status = HTTP.status200
      let headers = []
      let body = BSL8.pack (show plops) <> ".\n"
      respond (Wai.responseLBS status headers body)
  else do
    -- lets assume this is get and try and access the API
    response <- API.runAPI projection api (Wai.pathInfo request)
    case response of
      Just a -> respond (Wai.responseLBS HTTP.status400 [] (JSON.encode a))
      Nothing ->
        respond (Wai.responseLBS HTTP.status400 [] "No response!")



