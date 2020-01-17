{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MiniEventStore.Types.Instances where

import qualified Data.Aeson as JSON
import qualified MiniEventStore.EventStore as EventStore
import MiniEventStore.Types.Internal

instance
  ( JSON.FromJSON action,
    GetEvents m,
    CacheState state m
  ) =>
  RunAPI action state m
  where
  runAPIRequest projection' api' as = do
    (_, state') <- runProjection projection'
    pure $ api' state' as

instance
  ( Monad m,
    CacheState state m,
    GetEvents m,
    JSON.FromJSON action
  ) =>
  RunProjection action state m
  where
  runProjection projection' = do
    (oldIndex, oldState) <- getState @state
    events' <- getEvents oldIndex
    let (newIndex, newState) = EventStore.runProjectionFromJSON events' oldIndex oldState projection'
    putState newIndex newState
    pure (newIndex, newState)
