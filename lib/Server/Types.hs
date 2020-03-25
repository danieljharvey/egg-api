{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types where

import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Egg.EggM as Egg
import qualified MiniEventStore as MES
import Servant

type PostAPI action =
  ReqBody '[JSON] action :> Post '[JSON] String

-- api for posting new events
postAPI ::
  (JSON.ToJSON action) =>
  Egg.EggConfig action state ->
  Server (PostAPI action)
postAPI config = savePostEndpoint config

savePostEndpoint ::
  (JSON.ToJSON action) =>
  Egg.EggConfig action state ->
  action ->
  Handler String
savePostEndpoint config action = do
  let bs8 = BSL.toStrict $ JSON.encode action
  resp <- liftIO $ runReaderT (Egg.runEggM (savePost bs8)) config
  Handler (pure resp)

-- post means plop an event in the store
savePost ::
  ( MES.WriteEvent m
  ) =>
  BS8.ByteString ->
  m String
savePost jsonStr = do
  MES.writeEvent jsonStr
  pure "Saved!"
