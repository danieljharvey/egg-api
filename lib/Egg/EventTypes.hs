{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Egg.EventTypes where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Test.QuickCheck.Arbitrary

--

newtype TileId
  = TileId {getTileId :: Int}
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON, Arbitrary)

newtype BoardId
  = BoardId {getBoardId :: Int}
  deriving (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

data NewBoard
  = NewBoard
      { width :: Int,
        height :: Int,
        boardId :: BoardId
      }

instance FromJSON NewBoard where
  parseJSON = withObject "newBoard" $ \o -> do
    type' <- o .: "type"
    when ((type' :: String) /= "new_board") $ fail "Wrong type"
    width' <- o .: "width"
    height' <- o .: "height"
    boardId' <- o .: "boardId"
    return $ NewBoard width' height' boardId'

--

data AddTile
  = AddTile
      { x :: Int,
        y :: Int,
        tileId :: TileId,
        tileBoardId :: BoardId
      }

instance FromJSON AddTile where
  parseJSON = withObject "addTile" $ \o -> do
    type' <- o .: "type"
    when ((type' :: String) /= "add_tile") $ fail "Wrong type"
    x' <- o .: "x"
    y' <- o .: "y"
    tileId' <- o .: "tileId"
    boardId' <- o .: "boardId"
    return $ AddTile x' y' tileId' boardId'

data BoardActions
  = NewBoardAction NewBoard
  | AddTileAction AddTile

instance FromJSON BoardActions where
  parseJSON a =
    (NewBoardAction <$> parseJSON a)
      <|> (AddTileAction <$> parseJSON a)
