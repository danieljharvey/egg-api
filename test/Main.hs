{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
            runTestEggM' (InternalTestState 10 [] $ EventStore.def testProjection) $
              fst <$> Egg.getState @Integer
      fst val `shouldBe` 10
    it "Passes through events" $ do
      let val = runTestEggM' (InternalTestState 0 [JSON.toJSON Reset] $ EventStore.def testProjection) $ Egg.getEvents
      fst val `shouldBe` Map.fromList [(1, JSON.toJSON Reset)]
    it "Writes an event" $ do
      let val = runTestEggM' (InternalTestState 0 [] $ EventStore.def testProjection) $ do
            Egg.writeEvent (toStrict $ JSON.encode Up)
            Egg.writeEvent (toStrict $ JSON.encode Down)
            Egg.getEvents
      fst val `shouldBe` Map.fromList [(1, JSON.toJSON Up), (2, JSON.toJSON Down)]
    it "Runs a projection" $ do
      let val = runTestEggM' (InternalTestState 0 [] $ EventStore.def testProjection) $ do
            Egg.writeEvent (toStrict $ JSON.encode Up)
            Egg.writeEvent (toStrict $ JSON.encode Down)
            Egg.writeEvent (toStrict $ JSON.encode Up)
            projection' <- asks Egg.projection
            Egg.runProjection projection'
      -- whats the answer?
      (snd . fst) val `shouldBe` 1
      -- whats the next key?
      (iLastKeyUsed <$> snd) val `shouldBe` 4
    it "Runs a projection in parts gives same result" $ do
      let val = runTestEggM' (InternalTestState 0 [] $ EventStore.def testProjection) $ do
            projection' <- asks Egg.projection
            Egg.writeEvent (toStrict $ JSON.encode Up)
            _ <- Egg.runProjection projection'
            Egg.writeEvent (toStrict $ JSON.encode Down)
            _ <- Egg.runProjection projection'
            Egg.writeEvent (toStrict $ JSON.encode Up)
            Egg.runProjection projection'
      -- whats the answer?
      (snd . fst) val
        `shouldBe` 1
      -- whats the next key?
      (iLastKeyUsed <$> snd)
        val
        `shouldBe` 4

data TestAction
  = Up
  | Down
  | Reset
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

testProjection :: EventStore.Projection TestAction Integer
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
      { iLastKeyUsed :: Integer,
        iAllEvents :: [JSON.Value],
        iState :: s
      }

runTestEggM' ::
  InternalTestState Integer ->
  TestEggM Integer a ->
  (a, InternalTestState Integer)
runTestEggM' as val = do
  runState state' as
  where
    state' =
      runReaderT (runTestEggM val) stateConfig

newtype TestEggM state t
  = TestEggM
      { runTestEggM ::
          ReaderT (Egg.EggConfig TestAction state)
            (State (InternalTestState Integer))
            t
      }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Egg.EggConfig TestAction state),
      MonadState (InternalTestState Integer)
    )

instance Egg.GetEvents (TestEggM state) where
  getEvents =
    Map.fromList <$> zip [(1 :: Integer) ..] <$> iAllEvents <$> get

instance Egg.WriteEvent (TestEggM state) where
  writeEvent bs =
    modify $
      ( \(InternalTestState i es s) ->
          case JSON.decode (fromStrict bs) of
            Just action -> (InternalTestState i (es <> [action]) s)
            Nothing -> InternalTestState i es s
      )

instance Egg.CacheState Integer (TestEggM state) where

  putState lastIndex newState = do
    (InternalTestState _ es _) <- get
    put (InternalTestState lastIndex es newState)

  getState = do
    (InternalTestState lastIndex _ state') <- get
    pure (lastIndex, state')

maxKey :: EventStore.EventList -> Integer
maxKey as = Map.foldrWithKey (\k _ k' -> max k k') 0 as

stateConfig ::
  Egg.EggConfig TestAction Integer
stateConfig =
  Egg.EggConfig
    { Egg.dbConnection = undefined,
      Egg.api = undefined,
      Egg.projection = testProjection,
      Egg.cachedState = undefined
    }
