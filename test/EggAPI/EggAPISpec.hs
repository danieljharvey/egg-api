{-# LANGUAGE OverloadedStrings #-}

module EggAPI.EggAPISpec where

-- tests of Egg API itself

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import qualified Egg.API as API
import Egg.EventTypes
import qualified Egg.SampleProjections as Sample
import qualified MiniEventStore as MES
import Test.Hspec
import Test.QuickCheck
import TestEggM

writeEvent' :: (JSON.ToJSON a, MES.WriteEvent m) => a -> m ()
writeEvent' a = MES.writeEvent (toStrict $ JSON.encode a)

emptyState' :: InternalTestState Sample.EggState
emptyState' =
  InternalTestState
    []
    (MES.LastRow 0, MES.def Sample.eggBoardProjection)

expectedState :: Sample.EggState
expectedState = regularState expectedBoard

regularState :: Sample.Board -> Sample.EggState
regularState a = Sample.EggState (Map.fromList [(BoardId 1, a)])

expectedBoard :: Sample.Board
expectedBoard =
  Sample.Board
    [ [TileId 0, TileId 1],
      [TileId 1, TileId 0]
    ]

expectedBoardResponse :: API.LevelResponse
expectedBoardResponse =
  API.LevelResponse
    (API.boardToTileBoard expectedBoard)
    (BoardId 1)
    (pure (BoardId 1))
    (API.BoardSize 2 2)

fourRotateEvents :: Map.Map MES.EventId BoardActions
fourRotateEvents =
  Map.fromList
    [ (MES.EventId 1, RotateBoardAction $ RotateBoard 1 Clockwise),
      (MES.EventId 2, RotateBoardAction $ RotateBoard 1 Clockwise),
      (MES.EventId 3, RotateBoardAction $ RotateBoard 1 Clockwise),
      (MES.EventId 4, RotateBoardAction $ RotateBoard 1 Clockwise)
    ]

backAndForthRotate :: Map.Map MES.EventId BoardActions
backAndForthRotate =
  Map.fromList
    [ (MES.EventId 1, RotateBoardAction $ RotateBoard 1 Clockwise),
      (MES.EventId 2, RotateBoardAction $ RotateBoard 1 AntiClockwise)
    ]

eggAPISpec :: Spec
eggAPISpec = do
  describe "EggAPI" $ do
    it "Encodes and decodes TileId" $ property $
      \a -> (JSON.fromJSON . JSON.toJSON) a == JSON.Success (a :: TileId)
    it "Encodes and decodes Board" $ property $
      \a -> (JSON.fromJSON . JSON.toJSON) a == JSON.Success (a :: Sample.Board)
    it "Encodes and decodes BoardActions" $ property $
      \a -> (JSON.fromJSON . JSON.toJSON) a == JSON.Success (a :: BoardActions)
  describe "Projection" $ do
    it "Does the projection manually" $ do
      let events =
            Map.fromList
              [ (MES.EventId 1, NewBoardAction $ NewBoard 2 2 1),
                (MES.EventId 2, AddTileAction $ AddTile 0 1 1 1),
                (MES.EventId 3, AddTileAction $ AddTile 1 0 1 1)
              ]
      let (lastRow, state) =
            MES.runProjectionInternal
              events
              (MES.LastRow 0)
              Sample.empty
              Sample.eggBoardProjection
      state `shouldBe` expectedState
      lastRow `shouldBe` (MES.LastRow 3)
    it "Expands the board" $ do
      let events =
            Map.fromList
              [ (MES.EventId 1, NewBoardAction $ NewBoard 2 2 1),
                (MES.EventId 2, ExpandBoardAction $ ExpandBoard 1)
              ]
      let (lastRow, state) =
            MES.runProjectionInternal
              events
              (MES.LastRow 0)
              Sample.empty
              Sample.eggBoardProjection
      state `shouldBe` regularState (Sample.blankBoard (Sample.BoardSize 3))
      lastRow `shouldBe` (MES.LastRow 2)
    it "Doesn't shrink a 1x1 board" $ do
      let events =
            Map.fromList
              [ (MES.EventId 1, NewBoardAction $ NewBoard 1 1 1),
                (MES.EventId 2, ShrinkBoardAction $ ShrinkBoard 1)
              ]
      let (lastRow, state) =
            MES.runProjectionInternal
              events
              (MES.LastRow 0)
              Sample.empty
              Sample.eggBoardProjection
      state `shouldBe` regularState (Sample.blankBoard (Sample.BoardSize 1))
      lastRow `shouldBe` (MES.LastRow 2)
    it "Shrinks the board" $ do
      let events =
            Map.fromList
              [ (MES.EventId 1, NewBoardAction $ NewBoard 3 3 1),
                (MES.EventId 2, ShrinkBoardAction $ ShrinkBoard 1)
              ]
      let (lastRow, state) =
            MES.runProjectionInternal
              events
              (MES.LastRow 0)
              Sample.empty
              Sample.eggBoardProjection
      state `shouldBe` regularState (Sample.blankBoard (Sample.BoardSize 2))
      lastRow `shouldBe` (MES.LastRow 2)
    it "Four rotates leaves everything the same" $ property $
      \a -> do
        let (_, state) =
              MES.runProjectionInternal
                fourRotateEvents
                (MES.LastRow 0)
                (regularState a)
                Sample.eggBoardProjection
        state == (regularState a)
    it "Back and forth rotate ends up the same" $ property $
      \a -> do
        let (_, state) =
              MES.runProjectionInternal
                backAndForthRotate
                (MES.LastRow 0)
                (regularState a)
                Sample.eggBoardProjection
        state == (regularState a)
