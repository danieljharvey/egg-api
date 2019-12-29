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

import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import qualified Egg.SampleProjections as Sample
import Egg.Types.Instances ()
import Egg.Types.Internal
import GHC.Generics
import Test.Hspec
import Test.QuickCheck
import TestEggM

emptyState :: InternalTestState Integer
emptyState =
  InternalTestState
    []
    (NextRow 0, def testProjection)

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
            runTestEggM' (InternalTestState [] (NextRow 10, def testProjection)) $
              fst <$> getState @Integer
      fst val `shouldBe` (NextRow 10)
    it "Passes through events" $ do
      let val =
            runTestEggM'
              ( InternalTestState
                  [JSON.toJSON Reset]
                  ( NextRow 0,
                    def testProjection
                  )
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
      (fst . fst) val `shouldBe` (NextRow 4)
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
      (fst <$> fst)
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

-- basic test data

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

testAPI :: API Integer
testAPI state' as =
  case as of
    ["test"] -> Just (JSON.String "yo")
    ["doubled"] -> Just (JSON.Number $ fromIntegral (state' * 2))
    _ -> Nothing
