{-# LANGUAGE OverloadedStrings #-}

module MiniEventStore.EventStore where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import MiniEventStore.Types.Internal

type ActionList action =
  M.Map EventId action

-- run all events
runProjectionFromJSON ::
  (JSON.FromJSON action) =>
  EventList ->
  LastRow ->
  state ->
  Projection action state ->
  (LastRow, state)
runProjectionFromJSON =
  runProjectionInternal . convertActions
  where
    convertActions =
      M.mapMaybe
        (resultToMaybe . JSON.fromJSON)

runProjectionInternal ::
  ActionList action ->
  LastRow ->
  state ->
  Projection action state ->
  (LastRow, state)
runProjectionInternal actions startKey oldState projection' =
  ((nextKey actions), newState)
  where
    newState =
      foldl (flip $ reducer projection') oldState (usefulEvents actions)
    filterOldEvents =
      M.filterWithKey (\k _ -> getEventId k > getLastRow startKey)
    usefulEvents =
      M.elems . filterOldEvents

resultToMaybe :: JSON.Result a -> Maybe a
resultToMaybe a =
  case a of
    JSON.Success a' -> Just a'
    _ -> Nothing

nextKey :: M.Map EventId a -> LastRow
nextKey =
  LastRow
    . getEventId
    . (fromMaybe (EventId 0))
    . listToMaybe
    . reverse
    . M.keys
