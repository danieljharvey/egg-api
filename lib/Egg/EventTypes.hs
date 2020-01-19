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
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

--

newtype TileId
  = TileId {getTileId :: Int}
  deriving (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Arbitrary, Enum, Real, Num, Integral)

---

data Tile
  = Tile {id :: TileId}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

---

newtype BoardId
  = BoardId {getBoardId :: Int}
  deriving (Eq, Ord, Show)
  deriving newtype
    ( ToJSON,
      FromJSON,
      ToJSONKey,
      FromJSONKey,
      Enum,
      Arbitrary,
      Num,
      Real,
      Integral
    )

---

data RotateDirection
  = Clockwise
  | AntiClockwise
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary RotateDirection where
  arbitrary = genericArbitrary

---

data NewBoard
  = NewBoard
      { width :: Int,
        height :: Int,
        boardId :: BoardId
      }
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary NewBoard where
  arbitrary = genericArbitrary

instance FromJSON NewBoard where
  parseJSON = withObject "newBoard" $ \o -> do
    type' <- o .: "type"
    when ((type' :: String) /= "new_board") $ fail "Wrong type"
    width' <- o .: "width"
    height' <- o .: "height"
    boardId' <- o .: "boardId"
    return $ NewBoard width' height' boardId'

instance ToJSON NewBoard where
  toJSON (NewBoard width' height' boardId') =
    object
      [ "type" .= String "new_board",
        "width" .= Number (fromIntegral width'),
        "height" .= Number (fromIntegral height'),
        "boardId" .= Number (fromIntegral boardId')
      ]

---

data AddTile
  = AddTile
      { x :: Int,
        y :: Int,
        addTileId :: TileId,
        tileBoardId :: BoardId
      }
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary AddTile where
  arbitrary = genericArbitrary

instance FromJSON AddTile where
  parseJSON = withObject "addTile" $ \o -> do
    type' <- o .: "type"
    when ((type' :: String) /= "add_tile") $ fail "Wrong type"
    x' <- o .: "x"
    y' <- o .: "y"
    tileId' <- o .: "tileId"
    boardId' <- o .: "boardId"
    return $ AddTile x' y' tileId' boardId'

instance ToJSON AddTile where
  toJSON (AddTile x' y' tileId' tileBoardId') =
    object
      [ "type" .= String "add_tile",
        "x" .= Number (fromIntegral x'),
        "y" .= Number (fromIntegral y'),
        "tileId" .= Number (fromIntegral tileId'),
        "boardId" .= Number (fromIntegral tileBoardId')
      ]

---

data ExpandBoard
  = ExpandBoard
      { expandBoardId :: BoardId
      }
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary ExpandBoard where
  arbitrary = genericArbitrary

instance FromJSON ExpandBoard where
  parseJSON = withObject "expandBoard" $ \o -> do
    type' <- o .: "type"
    when ((type' :: String) /= "expand_board") $ fail "Wrong type"
    boardId' <- o .: "boardId"
    return $ ExpandBoard boardId'

instance ToJSON ExpandBoard where
  toJSON (ExpandBoard expandBoardId') =
    object
      [ "type" .= String "expand_board",
        "boardId" .= Number (fromIntegral expandBoardId')
      ]

---

data ShrinkBoard
  = ShrinkBoard
      { shrinkBoardId :: BoardId
      }
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary ShrinkBoard where
  arbitrary = genericArbitrary

instance FromJSON ShrinkBoard where
  parseJSON = withObject "shrinkBoard" $ \o -> do
    type' <- o .: "type"
    when ((type' :: String) /= "shrink_board") $ fail "Wrong type"
    boardId' <- o .: "boardId"
    return $ ShrinkBoard boardId'

instance ToJSON ShrinkBoard where
  toJSON (ShrinkBoard shrinkBoardId') =
    object
      [ "type" .= String "shrink_board",
        "boardId" .= Number (fromIntegral shrinkBoardId')
      ]

---

data RotateBoard
  = RotateBoard
      { rotateBoardId :: BoardId,
        rotateDirection :: RotateDirection
      }
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary RotateBoard where
  arbitrary = genericArbitrary

instance FromJSON RotateBoard where
  parseJSON = withObject "rotateBoard" $ \o -> do
    type' <- o .: "type"
    when ((type' :: String) /= "rotate_board") $ fail "Wrong type"
    boardId' <- o .: "boardId"
    direction' <- o .: "direction"
    case (direction' :: String) of
      "clockwise" -> pure (RotateBoard boardId' Clockwise)
      "anticlockwise" -> pure (RotateBoard boardId' AntiClockwise)
      _ -> fail "Unknown direction"

instance ToJSON RotateBoard where
  toJSON (RotateBoard rotateBoardId' direction) =
    object
      [ "type" .= String "rotate_board",
        "boardId"
          .= Number
            (fromIntegral rotateBoardId'),
        "direction"
          .= String
            ( if direction == Clockwise
                then "clockwise"
                else "anticlockwise"
            )
      ]

---

data BoardActions
  = NewBoardAction NewBoard
  | AddTileAction AddTile
  | ExpandBoardAction ExpandBoard
  | ShrinkBoardAction ShrinkBoard
  | RotateBoardAction RotateBoard
  deriving (Eq, Ord, Show, Generic)

instance Arbitrary BoardActions where
  arbitrary = genericArbitrary

instance FromJSON BoardActions where
  parseJSON a =
    (NewBoardAction <$> parseJSON a)
      <|> (AddTileAction <$> parseJSON a)
      <|> (ExpandBoardAction <$> parseJSON a)
      <|> (ShrinkBoardAction <$> parseJSON a)
      <|> (RotateBoardAction <$> parseJSON a)

instance ToJSON BoardActions where
  toJSON (NewBoardAction a) = toJSON a
  toJSON (AddTileAction a) = toJSON a
  toJSON (ExpandBoardAction a) = toJSON a
  toJSON (ShrinkBoardAction a) = toJSON a
  toJSON (RotateBoardAction a) = toJSON a
