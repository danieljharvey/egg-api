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

import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Database.PostgreSQL.Simple as SQL
import MiniEventStore

data EggConfig action state
  = EggConfig
      { dbConnection :: SQL.Connection,
        api :: API state,
        projection :: Projection action state,
        cachedState :: IORef (LastRow, state)
      }

makeConfig ::
  JSON.FromJSON action =>
  SQL.Connection ->
  Projection action state ->
  API state ->
  IORef (LastRow, state) ->
  EggConfig action state
makeConfig c p api' ioRef =
  EggConfig
    { dbConnection = c,
      api = api',
      projection = p,
      cachedState = ioRef
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
  getEvents (LastRow lastRow) = do
    rows <- dbQuery "SELECT * FROM EVENTS where ID > ? ORDER BY id ASC" [lastRow]
    pure $ Map.fromList $ catMaybes (parseReply <$> rows)

instance WriteEvent (EggM action state) where
  writeEvent json =
    dbExecute "INSERT INTO events (info) VALUES (?);" [json]

instance CacheState state (EggM action state) where

  putState index state = do
    cachedState' <- asks cachedState
    liftIO $ writeIORef cachedState' (index, state)

  getState = do
    cachedState' <- asks cachedState
    liftIO $ readIORef cachedState'

{-modifyState f = do
  cachedState' <- asks cachedState
  liftIO $ modifyIORef cachedState' f
  liftIO $ readIORef cachedState'-}

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

parseReply ::
  JSON.FromJSON a =>
  (Int, JSON.Value) ->
  Maybe (EventId, a)
parseReply (i, json) =
  case JSON.fromJSON json of
    JSON.Success a' -> Just (EventId i, a')
    _ -> Nothing
