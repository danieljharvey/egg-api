{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Egg.API where

import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Egg.EggM as Egg
import Egg.EventTypes
import qualified Egg.SampleProjections as Sample
import GHC.Generics
import qualified MiniEventStore as MES
import Servant

data Reply
  = Reply {items :: [T.Text]}
  deriving (Generic, JSON.ToJSON)

data BoardSize
  = BoardSize
      { width :: Int,
        height :: Int
      }
  deriving (Generic, JSON.ToJSON)

newtype TileBoard
  = TileBoard {getTileBoard :: [[Tile]]}
  deriving (Eq, Show, Generic)
  deriving newtype (JSON.ToJSON, JSON.FromJSON)

boardToTileBoard :: Sample.Board -> TileBoard
boardToTileBoard (Sample.Board tiles) =
  TileBoard $ (fmap . fmap) Tile tiles

data LevelResponse
  = LevelResponse
      { board :: TileBoard,
        levelID :: BoardId,
        levels :: [BoardId],
        boardSize :: BoardSize
      }
  deriving (Generic, JSON.ToJSON)

type EggServerAPI state =
  StateAPI state
    :<|> "levels"
      :> LevelsAPI

eggServerAPI ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server (EggServerAPI Sample.EggState)
eggServerAPI config =
  stateServer config :<|> levelsServer config

----
--
type LevelsAPI =
  (GetLevelsAPI :<|> GetLevelAPI)

levelsServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server LevelsAPI
levelsServer config =
  getLevelsServer config :<|> getLevelServer config

---

type GetLevelsAPI = Get '[JSON] [BoardId]

getLevelsServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server GetLevelsAPI
getLevelsServer config = do
  state' <- liftIO $ runReaderT (Egg.runEggM getUpdatedProjectionState) config
  pure $ getLevelList state'

---

type GetLevelAPI = Capture "levelId" Int :> Get '[JSON] LevelResponse

getLevelServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server GetLevelAPI
getLevelServer config = \levelId -> do
  state' <- liftIO $ runReaderT (Egg.runEggM getUpdatedProjectionState) config
  case getLevel state' levelId of
    Just found -> pure found
    Nothing -> throwError $ err500 {errBody = "Level could not be found"}

-----

type StateAPI state = "state" :> Get '[JSON] state

stateServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action state ->
  Server (StateAPI state)
stateServer config =
  liftIO $ runReaderT (Egg.runEggM getUpdatedProjectionState) config

---

getUpdatedProjectionState ::
  ( JSON.FromJSON action,
    MES.GetEvents m,
    MES.CacheState state m,
    MonadReader (Egg.EggConfig action state) m
  ) =>
  m state
getUpdatedProjectionState = do
  projection' <- asks Egg.projection
  (_, state') <- MES.runProjection projection'
  pure state'

sampleAPI :: MES.API Sample.EggState
sampleAPI state args =
  case args of
    ["state"] -> Just . JSON.toJSON $ state
    ["levels"] -> Just . JSON.toJSON $ getLevelList state
    ["levels", levelId'] -> JSON.toJSON <$> getLevelOld state levelId'
    ["get", "some", "eggs"] -> Just . JSON.toJSON . Reply $ ["here are the eggs"]
    ["get", "some", a] -> Just . JSON.toJSON . Reply $ ["here are your", a]
    _ -> Just . JSON.toJSON . Reply $ args

type EggServerAPI state =
  StateAPI state
    :<|> "levels"
      :> LevelsAPI

eggServerAPI ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server (EggServerAPI Sample.EggState)
eggServerAPI config =
  stateServer config :<|> levelsServer config

----
--
type LevelsAPI =
  (GetLevelsAPI :<|> GetLevelAPI)

levelsServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server LevelsAPI
levelsServer config =
  getLevelsServer config :<|> getLevelServer config

---

type GetLevelsAPI = Get '[JSON] [BoardId]

getLevelsServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server GetLevelsAPI
getLevelsServer config = do
  state' <- liftIO $ runReaderT (Egg.runEggM getUpdatedProjectionState) config
  pure $ getLevelList state'

---

type GetLevelAPI = Capture "levelId" Int :> Get '[JSON] LevelResponse

getLevelServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action Sample.EggState ->
  Server GetLevelAPI
getLevelServer config = \levelId -> do
  state' <- liftIO $ runReaderT (Egg.runEggM getUpdatedProjectionState) config
  case getLevel state' levelId of
    Just found -> pure found
    Nothing -> throwError $ err500 {errBody = "Level could not be found"}

-----

type StateAPI state = "state" :> Get '[JSON] state

stateServer ::
  (JSON.FromJSON action) =>
  Egg.EggConfig action state ->
  Server (StateAPI state)
stateServer config =
  liftIO $ runReaderT (Egg.runEggM getUpdatedProjectionState) config

---

getUpdatedProjectionState ::
  ( JSON.FromJSON action,
    MES.GetEvents m,
    MES.CacheState state m,
    MonadReader (Egg.EggConfig action state) m
  ) =>
  m state
getUpdatedProjectionState = do
  projection' <- asks Egg.projection
  (_, state') <- MES.runProjection projection'
  pure state'

----
--
hush :: Either e a -> Maybe a
hush a = case a of
  Right a' -> Just a'
  _ -> Nothing

-- parse text, find level, good times
getLevelOld :: Sample.EggState -> T.Text -> Maybe LevelResponse
getLevelOld state levelIdString =
  (textToInt levelIdString)
    >>= getLevel'
    >>= makeResponse
  where
    getLevel' levelId' =
      ((,) levelId')
        <$> Map.lookup (BoardId levelId') (Sample.boards state)
    makeResponse (levelId', board') =
      pure $
        LevelResponse
          (boardToTileBoard board')
          (BoardId levelId')
          (getLevelList state)
          (getBoardSize board')

-- find level, good times
getLevel :: Sample.EggState -> Int -> Maybe LevelResponse
getLevel state levelId =
  getLevel' levelId
    >>= makeResponse
  where
    getLevel' levelId' =
      ((,) levelId')
        <$> Map.lookup (BoardId levelId') (Sample.boards state)
    makeResponse (levelId', board') =
      pure $
        LevelResponse
          (boardToTileBoard board')
          (BoardId levelId')
          (getLevelList state)
          (getBoardSize board')

getBoardSize :: Sample.Board -> BoardSize
getBoardSize (Sample.Board as) =
  BoardSize (length as) (length as)

getLevelList :: Sample.EggState -> [BoardId]
getLevelList state =
  Map.keys (Sample.boards state)
