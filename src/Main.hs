{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.IORef
import Data.Semigroup
import qualified Database.PostgreSQL.Simple as SQL
import qualified Egg.API as API
import qualified Egg.EggM as Egg
import qualified Egg.EventTypes as Actions
import qualified Egg.SampleProjections as Sample
import qualified MiniEventStore as MES
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant
import Server.Types
import qualified System.Envy as Envy

type EggAPI =
  PostAPI Actions.BoardActions
    :<|> API.EggServerAPI Sample.EggState

eggAPI :: Proxy EggAPI
eggAPI = Proxy

eggAPIServer ::
  Egg.EggConfig Actions.BoardActions Sample.EggState ->
  Server EggAPI
eggAPIServer config =
  postAPI config :<|> API.eggServerAPI config

eggApplication ::
  Egg.EggConfig Actions.BoardActions Sample.EggState ->
  Wai.Application
eggApplication config = serve eggAPI (eggAPIServer config)

main :: IO ()
main = do
  config' <- Envy.decode
  case config' of
    Nothing -> putStrLn "Could not read env vars"
    Just config -> do
      let settings = MES.makeSettings config
      connection <- SQL.connectPostgreSQL (MES.configDatabase config)
      MES.createSchema connection
      ioRef <-
        newIORef
          ( (MES.LastRow 0),
            MES.def Sample.eggBoardProjection
          )
      let eggConfig = Egg.makeConfig connection Sample.eggBoardProjection ioRef
      Warp.runSettings settings (application eggConfig)

application ::
  Egg.EggConfig Actions.BoardActions Sample.EggState ->
  Wai.Application
application config =
  corsMiddleware (eggApplication config)

-- allow GET and POST with JSON
corsMiddleware :: Wai.Middleware
corsMiddleware = cors (const $ Just policy)
  where
    sc = simpleCorsResourcePolicy
    policy =
      sc
        { corsMethods =
            (corsMethods sc) <> [methodGet, methodPost, methodOptions],
          corsRequestHeaders =
            (corsRequestHeaders sc) <> [hContentType]
        }
