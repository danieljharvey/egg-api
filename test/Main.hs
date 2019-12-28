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

import Control.Monad.State
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Map as Map
import Data.Semigroup
import qualified Egg.SampleProjections as Sample
import Egg.Types.Instances ()
import Egg.Types.Internal
import GHC.Generics
import Test.Hspec
import Test.QuickCheck

emptyState :: InternalTestState Integer
emptyState =
  InternalTestState
    (NextRow 0)
    []
    (def testProjection)

main :: IO ()
main = hspec $ do
  describe "Roundtrip JSON" $ do
    it "Encodes and decodes" $ property $
      \x -> (JSON.fromJSON . JSON.toJSON) x == JSON.Success (x :: Sample.Board)
  describe "Testing the test application" $ do
    it "Our test monad returns a value through all that mess" $ property $
      \x -> fst (runTestEggM' emptyState (pure x)) == (x :: String)
    it "Last projection value is passed through" $ do
      let val =
            runTestEggM' (InternalTestState (NextRow 10) [] $ def testProjection) $
              fst <$> getState @Integer
      fst val `shouldBe` (NextRow 10)
    it "Passes through events" $ do
      let val =
            runTestEggM'
              ( InternalTestState (NextRow 0) [JSON.toJSON Reset] $
                  def testProjection
              )
              $ getEvents (NextRow 0)
      fst val `shouldBe` Map.fromList [((EventId 1), JSON.toJSON Reset)]
    it "Writes an event" $ do
      let val = runTestEggM' emptyState $ do
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Down)
            getEvents (NextRow 0)
      fst val
        `shouldBe` Map.fromList
          [ ((EventId 1), JSON.toJSON Up),
            ((EventId 2), JSON.toJSON Down)
          ]
    it "Runs a projection" $ do
      let val = runTestEggM' emptyState $ do
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Down)
            writeEvent (toStrict $ JSON.encode Up)
            runProjection testProjection
      -- whats the answer?
      (snd . fst) val `shouldBe` 1
      -- whats the next key?
      (iLastKeyUsed <$> snd) val `shouldBe` (NextRow 4)
    it "Runs a projection in parts gives same result" $ do
      let val = runTestEggM' emptyState $ do
            writeEvent (toStrict $ JSON.encode Up)
            _ <- runProjection testProjection
            writeEvent (toStrict $ JSON.encode Down)
            _ <- runProjection testProjection
            writeEvent (toStrict $ JSON.encode Up)
            runProjection testProjection
      -- whats the answer?
      (snd . fst) val
        `shouldBe` 1
      -- whats the next key?
      (iLastKeyUsed <$> snd)
        val
        `shouldBe` (NextRow 4)
    it "Receives nothing for a stupid API call" $ do
      let (val, _) = runTestEggM' emptyState $ runAPIRequest testProjection testAPI ["load", "of", "rubbish"]
      val `shouldBe` Nothing
    it "Receives something for a test call" $ do
      let (val, _) =
            runTestEggM' emptyState $
              runAPIRequest testProjection testAPI ["test"]
      val `shouldBe` (Just (JSON.String "yo"))
    it "Uses state in the API call" $ do
      let (val, _) = runTestEggM' emptyState $ do
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Down)
            runAPIRequest testProjection testAPI ["doubled"]
      val `shouldBe` (Just (JSON.Number 6))
    it "Runs the projections before the API call" $ do
      let (index, _) = runTestEggM' emptyState $ do
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Up)
            writeEvent (toStrict $ JSON.encode Down)
            _ <- runAPIRequest testProjection testAPI ["Horses"]
            getState @(Integer)
      (fst index) `shouldBe` (NextRow 6)

data TestAction
  = Up
  | Down
  | Reset
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      JSON.FromJSON,
      JSON.ToJSON
    )

testProjection :: Projection TestAction Integer
testProjection =
  Projection
    { title = "Test Projection",
      reducer = \action i -> case action of
        Up -> i + 1
        Down -> i - 1
        Reset -> 0,
      def = 0
    }

data InternalTestState s
  = InternalTestState
      { iLastKeyUsed :: NextRow,
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
      runTestEggM val

newtype TestEggM state t
  = TestEggM
      { runTestEggM ::
          State (InternalTestState Integer)
            t
      }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (InternalTestState Integer)
    )

-- todo, only fetch relevant events
instance GetEvents (TestEggM state) where
  getEvents from' =
    Map.fromList
      <$> filter (\(i, _) -> getEventId i > getNextRow from')
      <$> zip (EventId <$> [(1 :: Int) ..])
      <$> iAllEvents
      <$> get

instance WriteEvent (TestEggM state) where
  writeEvent bs =
    modify $
      ( \(InternalTestState i es s) ->
          case JSON.decode (fromStrict bs) of
            Just action -> (InternalTestState i (es <> [action]) s)
            Nothing -> InternalTestState i es s
      )

instance CacheState Integer (TestEggM state) where

  putState lastIndex newState = do
    (InternalTestState _ es _) <- get
    put (InternalTestState lastIndex es newState)

  getState = do
    (InternalTestState lastIndex _ state') <- get
    pure (lastIndex, state')

testAPI :: API Integer
testAPI state' as =
  case as of
    ["test"] -> Just (JSON.String "yo")
    ["doubled"] -> Just (JSON.Number $ fromIntegral (state' * 2))
    _ -> Nothing
