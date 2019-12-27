{-# LANGUAGE OverloadedStrings #-}

module Egg.EventStore where

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
