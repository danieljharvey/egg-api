{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Text as Tx
import qualified Database.PostgreSQL.Simple as SQL
import qualified Egg.API as API
import qualified Egg.DB as DB
import qualified Egg.EggM as Egg
import qualified Egg.SampleProjections as Sample
import Egg.Types.Internal
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import qualified System.Envy as Envy

main :: IO ()
main = do
  config' <- Envy.decode
  case config' of
    Nothing -> putStrLn "Could not read env vars"
    Just config -> do
      let settings = DB.makeSettings config
      connection <- SQL.connectPostgreSQL (DB.configDatabase config)
      DB.createSchema connection
      ioRef <-
        newIORef
          ( (NextRow 0),
            def Sample.eggBoardProjection
          )
      let eggConfig = Egg.makeConfig connection Sample.eggBoardProjection API.sampleAPI ioRef
      Warp.runSettings settings (application eggConfig)

application ::
  (JSON.FromJSON action, Show state) =>
  Egg.EggConfig action state ->
  Wai.Application
application config =
  simpleCors $
    ( \request respond ->
        runReaderT (Egg.runEggM (requestHandler request)) config
          >>= respond
    )

requestHandler ::
  ( JSON.FromJSON action,
    Show state,
    MonadIO m,
    GetEvents m,
    CacheState state m,
    WriteEvent m,
    MonadReader (Egg.EggConfig action state) m
  ) =>
  Wai.Request ->
  m Wai.Response
requestHandler request =
  if Wai.requestMethod request == HTTP.methodPost
    then (liftIO $ Wai.requestBody request) >>= handlePostRequest
    else handleGetRequest (Wai.pathInfo request)

-- post means plop an event in the store
handlePostRequest ::
  ( WriteEvent m
  ) =>
  BS8.ByteString ->
  m Wai.Response
handlePostRequest jsonStr = do
  -- write the event
  writeEvent jsonStr
  let status = HTTP.status200
  let headers = []
  let body = "Saved!"
  pure (Wai.responseLBS status headers body)

handleGetRequest ::
  ( JSON.FromJSON action,
    GetEvents m,
    CacheState state m,
    MonadReader (Egg.EggConfig action state) m
  ) =>
  [Tx.Text] ->
  m Wai.Response
handleGetRequest args = do
  -- lets assume this is get and try and access the API
  projection' <- asks Egg.projection
  api' <- asks Egg.api
  response <- runAPIRequest projection' api' args
  case response of
    Just a ->
      pure
        ( Wai.responseLBS
            HTTP.status200
            [(HTTP.hContentType, "application/json")]
            (JSON.encode a)
        )
    Nothing ->
      pure
        ( Wai.responseLBS
            HTTP.status400
            []
            "No response!"
        )
