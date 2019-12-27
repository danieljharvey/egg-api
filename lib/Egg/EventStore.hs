{-# LANGUAGE OverloadedStrings #-}

module Egg.EventStore where

import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)

type EventList = M.Map Integer JSON.Value

-- basic Projection
data Projection action state
  = Projection
      { reducer :: (action -> state -> state),
        def :: state,
        title :: Text
      }

data StatefulProjection action state
  = StatefulProjection
      { projection :: Projection action state,
        value :: MVar (Integer, state)
      }

-- create a projection with the empty default value
createMVar ::
  MonadIO m =>
  Projection action state ->
  m (StatefulProjection action state)
createMVar projection' = do
  mvar <- liftIO $ newMVar (0, (def projection'))
  pure $
    StatefulProjection
      { projection = projection',
        value = mvar
      }

readProjection ::
  (MonadIO m) =>
  StatefulProjection a state ->
  m state
readProjection (StatefulProjection _ value') =
  snd <$> (liftIO $ readMVar value')

getMostRecentIndex ::
  MonadIO m => StatefulProjection a s -> m Integer
getMostRecentIndex (StatefulProjection _ value') =
  fst <$> (liftIO $ readMVar value')

runStatefulProjection ::
  (MonadIO m, JSON.FromJSON action) =>
  StatefulProjection action state ->
  EventList ->
  m state
runStatefulProjection proj@(StatefulProjection projection' _) events =
  modifyState proj (\(i, s) -> runProjection events i s projection')

modifyState ::
  (MonadIO m) =>
  StatefulProjection action state ->
  ((Integer, state) -> (Integer, state)) ->
  m state
modifyState (StatefulProjection _ value') f =
  liftIO $
    modifyMVar value' (\s -> pure (f s, snd (f s)))

-- run all events
runProjection ::
  (JSON.FromJSON action) =>
  EventList ->
  Integer ->
  state ->
  Projection action state ->
  (Integer, state)
runProjection events startKey oldState projection' =
  ((nextKey events), newState)
  where
    newState =
      foldr (reducer projection') oldState (usefulEvents events)
    filterOldEvents =
      M.filterWithKey (\k _ -> k >= startKey)
    usefulEvents =
      catMaybes
        . (map (resultToMaybe . JSON.fromJSON))
        . M.elems
        . filterOldEvents

resultToMaybe :: JSON.Result a -> Maybe a
resultToMaybe a =
  case a of
    JSON.Success a' -> Just a'
    _ -> Nothing

nextKey :: EventList -> Integer
nextKey =
  (+ 1)
    . (fromMaybe 0)
    . listToMaybe
    . reverse
    . M.keys
