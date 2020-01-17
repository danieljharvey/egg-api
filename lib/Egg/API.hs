{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Egg.API where

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Egg.EventTypes
import qualified Egg.SampleProjections as Sample
import GHC.Generics
import MiniEventStore

-- types for our API

-- let's not get too clever
-- we take the state and a request string
-- and maybe return something good

data Reply
  = Reply {items :: [T.Text]}
  deriving (Generic, JSON.ToJSON)

data BoardSize
  = BoardSize
      { width :: Int,
        height :: Int
      }
  deriving (Generic, JSON.ToJSON)

data LevelResponse
  = LevelResponse
      { board :: Sample.Board,
        levelID :: BoardId,
        levels :: [BoardId],
        boardSize :: BoardSize
      }
  deriving (Generic, JSON.ToJSON)

sampleAPI :: API Sample.EggState
sampleAPI state args =
  case args of
    ["state"] -> Just . JSON.toJSON $ state
    ["levels"] -> Just . JSON.toJSON $ getLevelList state
    ["levels", levelId'] -> JSON.toJSON <$> getLevel state levelId'
    ["get", "some", "eggs"] -> Just . JSON.toJSON . Reply $ ["here are the eggs"]
    ["get", "some", a] -> Just . JSON.toJSON . Reply $ ["here are your", a]
    _ -> Just . JSON.toJSON . Reply $ args

hush :: Either e a -> Maybe a
hush a = case a of
  Right a' -> Just a'
  _ -> Nothing

-- parse text, find level, good times
getLevel :: Sample.EggState -> T.Text -> Maybe LevelResponse
getLevel state levelIdString =
  (textToInt levelIdString)
    >>= getLevel'
    >>= makeResponse
  where
    textToInt =
      hush . (fmap fst) . T.decimal
    getLevel' levelId' =
      ((,) levelId')
        <$> Map.lookup (BoardId levelId') (Sample.boards state)
    makeResponse (levelId', board') =
      pure $
        LevelResponse
          board'
          (BoardId levelId')
          (getLevelList state)
          (getBoardSize board')

getBoardSize :: Sample.Board -> BoardSize
getBoardSize (Sample.Board as) =
  BoardSize (length as) (length as)

getLevelList :: Sample.EggState -> [BoardId]
getLevelList state =
  Map.keys (Sample.boards state)
