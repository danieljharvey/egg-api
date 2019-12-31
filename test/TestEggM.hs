{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestEggM where

import Control.Monad.State
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Map as Map
import Data.Semigroup
import Egg.Types.Instances ()
import Egg.Types.Internal

data InternalTestState s
  = InternalTestState
      { iAllEvents :: [JSON.Value],
        iState :: (NextRow, s)
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
      <$> filter (\(i, _) -> getEventId i >= getNextRow from')
      <$> zip (EventId <$> [(1 :: Int) ..])
      <$> iAllEvents
      <$> get

instance WriteEvent (TestEggM state) where
  writeEvent bs =
    modify $
      ( \(InternalTestState es s) ->
          case JSON.decode (fromStrict bs) of
            Just action -> (InternalTestState (es <> [action]) s)
            Nothing -> InternalTestState es s
      )

instance CacheState Integer (TestEggM state) where

  putState lastIndex newState = do
    (InternalTestState es _) <- get
    put (InternalTestState es (lastIndex, newState))

  getState = do
    (InternalTestState _ (lastIndex, state')) <- get
    pure (lastIndex, state')
