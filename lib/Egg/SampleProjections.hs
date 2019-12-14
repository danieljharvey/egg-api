{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Egg.SampleProjections where

import qualified Data.Aeson                as JSON
import qualified Data.Map                  as Map
import           Data.Semigroup
import           Egg.EventStore
import           Egg.EventTypes
import           GHC.Generics
import           Test.QuickCheck.Arbitrary

data Board
  = Board
      { tiles :: [[TileId]]
      }
  deriving (Eq, Show, Generic, JSON.ToJSON, JSON.FromJSON)

instance Arbitrary Board where
  arbitrary = Board <$> arbitrary

data EggState
  = EggState
      {boards :: Map.Map BoardId Board}
  deriving (Eq, Show)

empty :: EggState
empty = EggState mempty

blankBoard :: Int -> Int -> Board
blankBoard w h =
  Board {tiles = replicate w (replicate h item)}
  where
    item = TileId 0

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as
  = (\(i,a') -> f i a') <$> as'
  where
    as'
      = zip [0..] as

setElem :: Int -> Int -> a -> [[a]] -> [[a]]
setElem ax ay item old
  = mapWithIndex (\x' row -> if ax == x'
                             then changeRow row
                             else row) old
  where
    changeRow
      = mapWithIndex (\y' item' -> if ay == y'
                                   then item
                                   else item')

eggBoardProjection :: Projection BoardActions EggState
eggBoardProjection =
  Projection
    { reducer = \action (EggState boards') -> case action of
        NewBoardAction (NewBoard width' height' boardId') ->
          EggState
            ( Map.singleton boardId' (blankBoard width' height')
                <> boards'
            )
        AddTileAction (AddTile x' y' tileId' boardId') ->
          EggState
            ( Map.adjust
                ( \(Board a) ->
                    Board $ setElem x' y' tileId' a
                )
                boardId'
                boards'
            ),
      def = empty,
      title = "Egg board projection"
    }
