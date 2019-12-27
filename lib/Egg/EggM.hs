{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Egg.EggM where

import Control.Concurrent (MVar, modifyMVar_, takeMVar)
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
  (Integer, a)

data EggConfig action state
  = EggConfig
      { dbConnection :: SQL.Connection,
        api :: API.API state,
        projection :: EventStore.Projection action state,
        cachedState :: MVar (EventRow state)
      }

class (Monad m) => GetEvents m where
  getEvents :: m EventStore.EventList

class (Monad m) => WriteEvent m where
  writeEvent :: BS8.ByteString -> m ()

class
  ( MonadReader (EggConfig action state) m,
    Monad m
  ) =>
  RunAPI action state m where
  runAPIRequest :: [Tx.Text] -> m (Maybe JSON.Value)

class (Monad m) => CacheState state m where

  putState :: Integer -> state -> m ()

  getState :: m (Integer, state)

class (Monad m) => RunProjection action state m where
  runProjection ::
    EventStore.Projection action state ->
    m (Integer, state)

makeConfig ::
  JSON.FromJSON action =>
  SQL.Connection ->
  EventStore.Projection action state ->
  API.API state ->
  MVar (EventRow state) ->
  EggConfig action state
makeConfig c p api' mVar =
  EggConfig
    { dbConnection = c,
      api = api',
      projection = p,
      cachedState = mVar
    }

newtype EggM action state t
  = EggM {runEggM :: ReaderT (EggConfig action state) IO t}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (EggConfig action state)
    )

instance GetEvents (EggM action state) where
  getEvents = getEvents'

instance WriteEvent (EggM action state) where
  writeEvent = writeEvent'

instance
  ( JSON.FromJSON action,
    GetEvents m,
    MonadReader (EggConfig action state) m,
    CacheState state m
  ) =>
  RunAPI action state m
  where
  runAPIRequest as = do
    projection' <- asks projection
    (_, state') <- runProjection projection'
    api' <- asks api
    pure $ api' state' as

instance CacheState state (EggM action state) where

  putState index state = do
    cachedState' <- asks cachedState
    liftIO $ modifyMVar_ cachedState' (\_ -> pure (index, state))

  getState = do
    cachedState' <- asks cachedState
    liftIO $ takeMVar cachedState'

instance
  ( Monad m,
    CacheState state m,
    GetEvents m,
    JSON.FromJSON action
  ) =>
  RunProjection action state m
  where
  runProjection projection' = do
    (index, state') <- getState @state
    events' <- getEvents
    let (newIndex, newState) = EventStore.runProjection events' index state' projection'
    putState newIndex newState
    pure (newIndex, newState)

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
getEvents' :: forall action state. EggM action state EventStore.EventList
getEvents' = do
  (lastIndex, _) <- getState @state
  rows <- dbQuery "SELECT * FROM EVENTS where ID >= ?" [lastIndex]
  pure $ Map.fromList $ catMaybes (parseReply <$> rows)

parseReply :: JSON.FromJSON a => (Integer, JSON.Value) -> Maybe (Integer, a)
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
