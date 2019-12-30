module EggAPI.EggAPISpec where

-- tests of Egg API itself

import qualified Data.Aeson as JSON
import qualified Egg.SampleProjections as Sample
import Egg.Types.Instances ()
import Test.Hspec
import Test.QuickCheck

eggAPISpec :: Spec
eggAPISpec = do
  describe "EggAPI" $ do
    it "Encodes and decodes" $ property $
      \x -> (JSON.fromJSON . JSON.toJSON) x == JSON.Success (x :: Sample.Board)
