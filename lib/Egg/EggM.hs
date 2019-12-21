{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Egg.EggM where

import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as Tx
import qualified Database.PostgreSQL.Simple as SQL
import qualified Egg.API as API
import qualified Egg.EventStore as EventStore

type EventRow a =
  (Int, a)

data EggConfig m action state
  = EggConfig
      { dbConnection :: SQL.Connection,
        writeEvent :: BS8.ByteString -> m (),
        -- getEvents :: m EventStore.EventList,
        runAPI :: [Tx.Text] -> m (Maybe JSON.Value),
        runProjection :: EventStore.EventList -> m state,
        getMostRecentIndex :: m Int
      }

class (Monad m) => GetEvents m where
  getEvents :: m EventStore.EventList

makeConfig ::
  JSON.FromJSON action =>
  SQL.Connection ->
  EventStore.StatefulProjection action state ->
  API.API state ->
  EggConfig (EggM action state) action state
makeConfig c p api' =
  EggConfig
    { dbConnection = c,
      writeEvent = writeEvent',
      -- getEvents = getEvents',
      runAPI = API.runAPI p api',
      runProjection = EventStore.runStatefulProjection p,
      getMostRecentIndex = EventStore.getMostRecentIndex p
    }

newtype EggM action state t
  = EggM {runEggM :: ReaderT (EggConfig (EggM action state) action state) IO t}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (EggConfig (EggM action state) action state)
    )

instance GetEvents (EggM action state) where
  getEvents = getEvents'

dbExecute ::
  (SQL.ToRow q) =>
  SQL.Query ->
  q ->
  EggM a s ()
dbExecute query args = do
  connection' <- asks dbConnection
  _ <- liftIO $ SQL.execute connection' query args
  pure ()

dbQuery :: (SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> EggM a s [r]
dbQuery query args = do
  connection' <- asks dbConnection
  liftIO $ SQL.query connection' query args

-- get the relevant events
getEvents' :: EggM a s EventStore.EventList
getEvents' = do
  lastIndex <- join (asks getMostRecentIndex)
  rows <- dbQuery "SELECT * FROM EVENTS where ID >= ?" [lastIndex]
  pure $ Map.fromList $ catMaybes (parseReply <$> rows)

parseReply :: JSON.FromJSON a => (Int, JSON.Value) -> Maybe (Int, a)
parseReply (i, json) =
  case JSON.fromJSON json of
    JSON.Success a' -> Just (i, a')
    _ -> Nothing

-- write an event to the store
writeEvent' ::
  BS8.ByteString ->
  EggM a s ()
writeEvent' json =
  dbExecute "INSERT INTO events (info) VALUES (?);" [json]
