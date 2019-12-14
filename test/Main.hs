module Main where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Aeson            as JSON
import qualified Egg.SampleProjections as Sample

main :: IO ()
main = hspec $ do
  describe "Roundtrip JSON" $ do
      it "Encodes and decodes" $ property $
        \x -> (JSON.fromJSON . JSON.toJSON) x == JSON.Success (x :: Sample.Board)

