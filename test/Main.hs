{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Map as Map
import Data.Semigroup
import qualified Egg.EggM as Egg
import qualified Egg.EventStore as EventStore
import qualified Egg.SampleProjections as Sample
import GHC.Generics
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Roundtrip JSON" $ do
    it "Encodes and decodes" $ property $
      \x -> (JSON.fromJSON . JSON.toJSON) x == JSON.Success (x :: Sample.Board)
  describe "Testing the test application" $ do
    it "Our test monad returns a value through all that mess" $ property $
      \x -> fst (runTestEggM' (InternalTestState 0 [] $ EventStore.def testProjection) (pure x)) == (x :: String)
    it "Last projection value is passed through" $ do
      let val =
            runTestEggM' (InternalTestState 10 [] $ EventStore.def testProjection)
              $ join
              $ asks Egg.getMostRecentIndex
      fst val `shouldBe` 10
    it "Passes through events" $ do
      let val = runTestEggM' (InternalTestState 0 [JSON.toJSON Reset] $ EventStore.def testProjection) $ (join (asks Egg.getEvents))
      fst val `shouldBe` Map.fromList [(1, JSON.toJSON Reset)]
    it "Writes an event" $ do
      let val = runTestEggM' (InternalTestState 0 [] $ EventStore.def testProjection) $ do
            writeEvent <- asks Egg.writeEvent
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Down)
            join (asks Egg.getEvents)
      fst val `shouldBe` Map.fromList [(1, JSON.toJSON Up), (2, JSON.toJSON Down)]
    it "Runs a projection" $ do
      let val = runTestEggM' (InternalTestState 0 [] $ EventStore.def testProjection) $ do
            writeEvent <- asks Egg.writeEvent
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Down)
            writeEvent (toStrict $ JSON.encode Up)
            events <- join (asks Egg.getEvents)
            runProjection' <- asks Egg.runProjection
            runProjection' events
      -- whats the answer?
      fst val `shouldBe` 1
      -- whats the next key?
      (iLastKeyUsed <$> snd) val `shouldBe` 4
    it "Runs a projection in parts gives same result" $ do
      let val = runTestEggM' (InternalTestState 0 [] $ EventStore.def testProjection) $ do
            writeEvent <- asks Egg.writeEvent
            runProjection' <- asks Egg.runProjection
            getEvents <- asks Egg.getEvents
            writeEvent (toStrict $ JSON.encode Up)
            _ <- getEvents >>= runProjection'
            writeEvent (toStrict $ JSON.encode Down)
            _ <- getEvents >>= runProjection'
            writeEvent (toStrict $ JSON.encode Up)
            getEvents >>= runProjection'
      -- whats the answer?
      fst val `shouldBe` 1
      -- whats the next key?
      (iLastKeyUsed <$> snd) val `shouldBe` 4

data TestAction
  = Up
  | Down
  | Reset
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

testProjection :: EventStore.Projection TestAction Int
testProjection =
  EventStore.Projection
    { EventStore.title = "Test Projection",
      EventStore.reducer = \action i -> case action of
        Up -> i + 1
        Down -> i - 1
        Reset -> 0,
      EventStore.def = 0
    }

data InternalTestState s
  = InternalTestState
      { iLastKeyUsed :: Int,
        iAllEvents :: [JSON.Value],
        iState :: s
      }

runTestEggM' :: InternalTestState Int -> TestEggM Int a -> (a, InternalTestState Int)
runTestEggM' as val = do
  runState state' as
  where
    state' =
      runReaderT (runTestEggM val) stateConfig

newtype TestEggM state t
  = TestEggM
      { runTestEggM ::
          ReaderT (Egg.EggConfig (TestEggM state) TestAction state)
            (State (InternalTestState Int))
            t
      }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Egg.EggConfig (TestEggM state) TestAction state),
      MonadState (InternalTestState Int)
    )

maxKey :: EventStore.EventList -> Int
maxKey as = Map.foldrWithKey (\k _ k' -> max k k') 0 as

stateConfig ::
  Egg.EggConfig (TestEggM state) TestAction Int
stateConfig =
  Egg.EggConfig
    { Egg.dbConnection = undefined,
      Egg.writeEvent = \bs ->
        modify $
          ( \(InternalTestState i es s) ->
              case JSON.decode (fromStrict bs) of
                Just action -> (InternalTestState i (es <> [action]) s)
                Nothing -> InternalTestState i es s
          ),
      Egg.getEvents =
        Map.fromList <$> zip [(1 :: Int) ..] <$> iAllEvents <$> get,
      Egg.runAPI = \_tx -> pure Nothing,
      Egg.runProjection = \events -> do
        (InternalTestState startAt es oldState) <- get
        let (lastKeyUsed, newState) = EventStore.runProjection events startAt oldState testProjection
        put (InternalTestState lastKeyUsed es newState)
        pure newState,
      Egg.getMostRecentIndex = iLastKeyUsed <$> get
    }
