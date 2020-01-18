{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Egg.SampleProjections where

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import Data.Semigroup
import Egg.EventTypes
import GHC.Generics
import MiniEventStore
import Test.QuickCheck.Arbitrary

data Board
  = Board
      { tiles :: [[TileId]]
      }
  deriving (Eq, Show, Generic, JSON.ToJSON, JSON.FromJSON)

instance Arbitrary Board where
  arbitrary = do
    items <- arbitrary
    pure . coordListToBoard . boardToCoordList $ Board items

data EggState
  = EggState
      {boards :: Map.Map BoardId Board}
  deriving (Eq, Show, Generic, JSON.ToJSON, JSON.FromJSON)

newtype BoardSize
  = BoardSize {getBoardSize :: Int}
  deriving (Eq, Ord, Show)

empty :: EggState
empty = EggState mempty

data Coord
  = Coord
      { coordX :: Int,
        coordY :: Int
      }
  deriving (Eq, Ord, Show, Generic)

blankBoard :: BoardSize -> Board
blankBoard (BoardSize w) =
  Board {tiles = replicate w (replicate w item)}
  where
    item = TileId 0

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as =
  (\(i, a') -> f i a') <$> as'
  where
    as' =
      zip [0 ..] as

setElem :: Coord -> TileId -> Board -> Board
setElem (Coord ax ay) item (Board old) =
  Board $
    mapWithIndex
      ( \x' row ->
          if ax == x'
            then changeRow row
            else row
      )
      old
  where
    changeRow =
      mapWithIndex
        ( \y' item' ->
            if ay == y'
              then item
              else item'
        )

boardToCoordList :: Board -> [(Coord, TileId)]
boardToCoordList (Board as) = concat withCoords
  where
    withCoords =
      mapWithIndex
        ( \x' row ->
            mapWithIndex (\y' item' -> ((Coord x' y'), item')) row
        )
        as

sizeFromCoordList :: [(Coord, TileId)] -> BoardSize
sizeFromCoordList as =
  BoardSize
    $ getMax
    $ foldMap Max ((sizeCoord . fst) <$> as)
  where
    sizeCoord :: Coord -> Int
    sizeCoord (Coord x' _) = x' + 1

coordListToBoard :: [(Coord, TileId)] -> Board
coordListToBoard as =
  foldl
    ( \board' (coord, tile) ->
        setElem coord tile board'
    )
    emptyBoard
    as
  where
    emptyBoard = blankBoard (sizeFromCoordList as)

dontNeedToRotate :: Board -> Bool
dontNeedToRotate (Board tiles') = length tiles' < 2

rotateBoard :: RotateDirection -> Board -> Board
rotateBoard direction board =
  if dontNeedToRotate board
    then board
    else coordListToBoard rotatedCoordList
  where
    boardSize' =
      sizeFromCoordList coordList
    coordList =
      boardToCoordList board
    rotatedCoordList =
      (\(coord, tile) -> (translateRotation boardSize' direction coord, tile)) <$> coordList

expandBoard :: [[TileId]] -> [[TileId]]
expandBoard as = (map (\a -> a <> [TileId 0]) as) <> [more]
  where
    more = (replicate (length as + 1) (TileId 0))

shrinkBoard :: [[a]] -> [[a]]
shrinkBoard as =
  map (\a -> take size a) (take size as)
  where
    size = max (length as - 1) 1

translateRotation ::
  BoardSize ->
  RotateDirection ->
  Coord ->
  Coord
translateRotation (BoardSize size') clockwise (Coord x' y') =
  case clockwise of
    Clockwise -> right
    AntiClockwise -> left
  where
    boardSize' =
      max (size' - 1) 1
    right =
      Coord (boardSize' - y') x'
    left =
      Coord y' (boardSize' - x')

eggBoardProjection :: Projection BoardActions EggState
eggBoardProjection =
  Projection
    { reducer = \action (EggState boards') -> case action of
        NewBoardAction (NewBoard width' _ boardId') ->
          EggState
            ( Map.singleton boardId' (blankBoard (BoardSize width'))
                <> boards'
            )
        AddTileAction (AddTile x' y' tileId' boardId') ->
          EggState
            ( Map.adjust
                ( \board' ->
                    setElem (Coord x' y') tileId' board'
                )
                boardId'
                boards'
            )
        ExpandBoardAction (ExpandBoard boardId') ->
          EggState
            ( Map.adjust
                ( \(Board a) ->
                    Board $ expandBoard a
                )
                boardId'
                boards'
            )
        ShrinkBoardAction (ShrinkBoard boardId') ->
          EggState
            ( Map.adjust
                ( \(Board a) ->
                    Board $ shrinkBoard a
                )
                boardId'
                boards'
            )
        RotateBoardAction (RotateBoard boardId' direction) ->
          EggState
            ( Map.adjust
                (\board' -> rotateBoard direction board')
                boardId'
                boards'
            ),
      def = empty,
      title = "Egg board projection"
    }
