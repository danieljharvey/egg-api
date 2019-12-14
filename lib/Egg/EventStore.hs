{-# LANGUAGE OverloadedStrings #-}

module Egg.EventStore where

import Control.Concurrent
  ( MVar,
    modifyMVar,
    newMVar,
    readMVar,
  )
import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    listToMaybe,
  )
import Data.Text (Text)

type EventList = M.Map Int JSON.Value

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
        value :: MVar (Int, state)
      }

-- create a projection with the empty default value
createMVar ::
  Projection action state ->
  IO (StatefulProjection action state)
createMVar projection' = do
  mvar <- newMVar (0, (def projection'))
  pure $
    StatefulProjection
      { projection = projection',
        value = mvar
      }

readProjection ::
  StatefulProjection a state ->
  IO state
readProjection (StatefulProjection _ value') =
  snd <$> readMVar value'

runStatefulProjection ::
  (JSON.FromJSON action) =>
  StatefulProjection action state ->
  EventList ->
  IO state
runStatefulProjection (StatefulProjection projection' value') events =
  modifyMVar
    value'
    ( \(startKey, oldState) -> do
        let newState = runProjection events startKey oldState projection'
        pure (newState, (snd newState))
    )

-- run all events
runProjection ::
  (JSON.FromJSON action) =>
  EventList ->
  Int ->
  state ->
  Projection action state ->
  (Int, state)
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

nextKey :: EventList -> Int
nextKey =
  (+ 1)
    . (fromMaybe 0)
    . listToMaybe
    . reverse
    . M.keys
