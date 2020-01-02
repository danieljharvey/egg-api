{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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
        iState :: (LastRow, s)
      }

runTestEggM' ::
  InternalTestState s ->
  TestEggM s a ->
  (a, InternalTestState s)
runTestEggM' as val = do
  runState state' as
  where
    state' =
      runTestEggM val

newtype TestEggM state t
  = TestEggM
      { runTestEggM ::
          State (InternalTestState state)
            t
      }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (InternalTestState state)
    )

-- todo, only fetch relevant events
instance GetEvents (TestEggM state) where
  getEvents from' =
    Map.fromList
      <$> filter (\(i, _) -> getEventId i > getLastRow from')
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

instance CacheState state (TestEggM state) where

  putState lastIndex newState = do
    (InternalTestState es _) <- get
    put (InternalTestState es (lastIndex, newState))

  getState = do
    (InternalTestState _ (lastIndex, state')) <- get
    pure (lastIndex, state')
{-modifyState f = do
  modify (\(InternalTestState es a) -> (InternalTestState es (f a)))
  (InternalTestState _ (lastIndex, state')) <- get
  pure (lastIndex, state')-}
