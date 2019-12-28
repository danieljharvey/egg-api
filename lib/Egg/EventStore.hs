{-# LANGUAGE OverloadedStrings #-}

module Egg.EventStore where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Egg.Types.Internal

-- run all events
runProjection ::
  (JSON.FromJSON action) =>
  EventList ->
  NextRow ->
  state ->
  Projection action state ->
  (NextRow, state)
runProjection events startKey oldState projection' =
  ((nextKey events), newState)
  where
    newState =
      foldr (reducer projection') oldState (usefulEvents events)
    filterOldEvents =
      M.filterWithKey (\k _ -> getEventId k >= getNextRow startKey)
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

nextKey :: EventList -> NextRow
nextKey =
  NextRow
    . (+ 1)
    . getEventId
    . (fromMaybe (EventId 0))
    . listToMaybe
    . reverse
    . M.keys
