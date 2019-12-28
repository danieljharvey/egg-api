{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Egg.Types.Internal where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Data.Text as Tx

-- Datatypes to build your own EventStore

newtype NextRow
  = NextRow {getNextRow :: Int}
  deriving (Eq, Ord, Show)

newtype EventId
  = EventId {getEventId :: Int}
  deriving (Eq, Ord, Show)

type EventList = M.Map EventId JSON.Value

-- basic Projection
data Projection action state
  = Projection
      { reducer :: (action -> state -> state),
        def :: state,
        title :: Tx.Text
      }

type API state =
  state -> [Tx.Text] -> Maybe JSON.Value

-- Grabs the list of events
class (Monad m) => GetEvents m where
  getEvents :: NextRow -> m EventList

-- Write a new event to the store
class (Monad m) => WriteEvent m where
  writeEvent :: BS8.ByteString -> m ()

-- run an API querying the current state
class
  ( Monad m,
    GetEvents m,
    CacheState state m
  ) =>
  RunAPI action state m where
  runAPIRequest ::
    Projection action state ->
    API state ->
    [Tx.Text] ->
    m (Maybe JSON.Value)

-- cache and retrieve a snapshotted state
class (Monad m) => CacheState state m where

  putState :: NextRow -> state -> m ()

  getState :: m (NextRow, state)

-- run a projection
class (Monad m) => RunProjection action state m where
  runProjection ::
    Projection action state ->
    m (NextRow, state)
