{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Egg.Types.Instances where

import qualified Data.Aeson as JSON
import qualified Egg.EventStore as EventStore
import Egg.Types.Internal

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
    (index, state') <- getState @state
    events' <- getEvents index
    let (newIndex, newState) = EventStore.runProjection events' index state' projection'
    putState newIndex newState
    pure (newIndex, newState)
