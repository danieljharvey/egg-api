{-# LANGUAGE OverloadedStrings #-}

module EggAPI.EggAPISpec where

-- tests of Egg API itself

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import qualified Egg.API as API
import qualified Egg.EventStore as EventStore
import Egg.EventTypes
import qualified Egg.SampleProjections as Sample
import Egg.Types.Instances ()
import Egg.Types.Instances ()
import Egg.Types.Internal
import Test.Hspec
import Test.QuickCheck
import TestEggM

writeEvent' :: (JSON.ToJSON a, WriteEvent m) => a -> m ()
writeEvent' a = writeEvent (toStrict $ JSON.encode a)

emptyState' :: InternalTestState Sample.EggState
emptyState' =
  InternalTestState
    []
    (LastRow 0, def Sample.eggBoardProjection)

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

eggAPISpec :: Spec
eggAPISpec = do
  describe "EggAPI" $ do
    it "Encodes and decodes Board" $ property $
      \a -> (JSON.fromJSON . JSON.toJSON) a == JSON.Success (a :: Sample.Board)
    it "Encodes and decodes BoardActions" $ property $
      \a -> (JSON.fromJSON . JSON.toJSON) a == JSON.Success (a :: BoardActions)
  describe "Projection" $ do
    it "Does the projection manually" $ do
      let events =
            Map.fromList
              [ (EventId 1, NewBoardAction $ NewBoard 2 2 1),
                (EventId 2, AddTileAction $ AddTile 0 1 1 1),
                (EventId 3, AddTileAction $ AddTile 1 0 1 1)
              ]
      let (lastRow, state) = EventStore.runProjectionInternal events (LastRow 0) Sample.empty Sample.eggBoardProjection
      state `shouldBe` expectedState
      lastRow `shouldBe` (LastRow 3)
    it "Makes a board and sets tiles" $ do
      let (state, (InternalTestState _ (lastRow, _))) = runTestEggM' emptyState' $ do
            writeEvent' $ NewBoard 2 2 1
            writeEvent' $ AddTile 0 1 1 1
            writeEvent' $ AddTile 1 0 1 1
            runAPIRequest Sample.eggBoardProjection API.sampleAPI ["levels", "1"]
      lastRow `shouldBe` (LastRow 3)
      state `shouldBe` Just (JSON.toJSON expectedBoard)
    it "Expands the board" $ do
      let events =
            Map.fromList
              [ (EventId 1, NewBoardAction $ NewBoard 2 2 1),
                (EventId 2, ExpandBoardAction $ ExpandBoard 1)
              ]
      let (lastRow, state) = EventStore.runProjectionInternal events (LastRow 0) Sample.empty Sample.eggBoardProjection
      state `shouldBe` regularState (Sample.blankBoard 3 3)
      lastRow `shouldBe` (LastRow 2)
    it "Doesn't shrink a 1x1 board" $ do
      let events =
            Map.fromList
              [ (EventId 1, NewBoardAction $ NewBoard 1 1 1),
                (EventId 2, ShrinkBoardAction $ ShrinkBoard 1)
              ]
      let (lastRow, state) = EventStore.runProjectionInternal events (LastRow 0) Sample.empty Sample.eggBoardProjection
      state `shouldBe` regularState (Sample.blankBoard 1 1)
      lastRow `shouldBe` (LastRow 2)
    it "Shrinks the board" $ do
      let events =
            Map.fromList
              [ (EventId 1, NewBoardAction $ NewBoard 3 3 1),
                (EventId 2, ShrinkBoardAction $ ShrinkBoard 1)
              ]
      let (lastRow, state) = EventStore.runProjectionInternal events (LastRow 0) Sample.empty Sample.eggBoardProjection
      state `shouldBe` regularState (Sample.blankBoard 2 2)
      lastRow `shouldBe` (LastRow 2)
