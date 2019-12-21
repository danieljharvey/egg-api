{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Egg.API where

import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Egg.EventStore as EventStore
import Egg.EventTypes
import qualified Egg.SampleProjections as Sample
import GHC.Generics

-- types for our API

-- let's not get too clever
-- we take the state and a request string
-- and maybe return something good

type API state =
  state -> [T.Text] -> Maybe JSON.Value

-- this grabs the state and does the bullshit
runAPI ::
  MonadIO m =>
  EventStore.StatefulProjection action state ->
  API state ->
  [T.Text] ->
  m (Maybe JSON.Value)
runAPI projection api requestPath = do
  state <- EventStore.readProjection projection
  pure (api state requestPath)

data Reply
  = Reply {items :: [T.Text]}
  deriving (Generic, JSON.ToJSON)

sampleAPI :: API Sample.EggState
sampleAPI state args =
  case args of
    ["levels", levelId] -> JSON.toJSON <$> getLevel state levelId
    ["get", "some", "eggs"] -> Just . JSON.toJSON . Reply $ ["here are the eggs"]
    ["get", "some", a] -> Just . JSON.toJSON . Reply $ ["here are your", a]
    _ -> Just . JSON.toJSON . Reply $ args

hush :: Either e a -> Maybe a
hush a = case a of
  Right a' -> Just a'
  _ -> Nothing

textToInt :: T.Text -> Maybe Int
textToInt = hush . (fmap fst) . T.decimal

-- parse text, find level, good times
getLevel :: Sample.EggState -> T.Text -> Maybe Sample.Board
getLevel state levelIdString =
  (textToInt levelIdString)
    >>= (\levelId -> Map.lookup (BoardId levelId) (Sample.boards state))
