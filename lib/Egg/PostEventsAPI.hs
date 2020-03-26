{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Egg.PostEventsAPI where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Egg.EggM as Egg
import Egg.EventTypes
import qualified MiniEventStore as MES
import Servant

type NewBoardActionAPI =
  ( "NewBoard"
      :> Capture "boardId" BoardId
      :> Post '[JSON] ()
  )

newBoardAction :: Egg.EggConfig BoardActions state -> Server NewBoardActionAPI
newBoardAction config =
  ( \boardId' ->
      liftIO $
        savePost
          config
          (NewBoardAction $ NewBoard 5 5 boardId')
  )

---

type AddTileActionAPI =
  ( "AddTile" :> Capture "boardId" BoardId
      :> Capture "tileId" TileId
      :> Capture "x" Int
      :> Capture "y" Int
      :> Post '[JSON] ()
  )

addTileAction :: Egg.EggConfig BoardActions state -> Server AddTileActionAPI
addTileAction config =
  ( \boardId' tileId' x' y' ->
      liftIO $
        savePost
          config
          (AddTileAction $ AddTile x' y' tileId' boardId')
  )

---

type ExpandBoardActionAPI =
  ("ExpandBoard" :> Capture "boardId" BoardId :> Post '[JSON] ())

expandBoardAction :: Egg.EggConfig BoardActions state -> Server ExpandBoardActionAPI
expandBoardAction config =
  ( \boardId' ->
      liftIO $
        savePost
          config
          ( ExpandBoardAction $
              ExpandBoard boardId'
          )
  )

---

type ShrinkBoardActionAPI =
  ("ShrinkBoard" :> Capture "boardId" BoardId :> Post '[JSON] ())

shrinkBoardAction :: Egg.EggConfig BoardActions state -> Server ShrinkBoardActionAPI
shrinkBoardAction config =
  ( \boardId' ->
      liftIO $
        savePost
          config
          ( ShrinkBoardAction $
              ShrinkBoard boardId'
          )
  )

---

type RotateBoardActionAPI =
  ( "RotateBoard" :> Capture "boardId" BoardId
      :> Capture "rotateDirection" RotateDirection
      :> Post '[JSON] ()
  )

rotateBoardAction ::
  Egg.EggConfig BoardActions state ->
  Server RotateBoardActionAPI
rotateBoardAction config =
  ( \boardId' direction' ->
      liftIO $
        savePost config (RotateBoardAction $ RotateBoard boardId' direction')
  )

---

type PostEventsAPI =
  NewBoardActionAPI
    :<|> AddTileActionAPI
    :<|> ExpandBoardActionAPI
    :<|> ShrinkBoardActionAPI
    :<|> RotateBoardActionAPI

postEvents :: Egg.EggConfig BoardActions state -> Server PostEventsAPI
postEvents config =
  newBoardAction config
    :<|> addTileAction config
    :<|> expandBoardAction config
    :<|> shrinkBoardAction config
    :<|> rotateBoardAction config

---

savePost ::
  (JSON.ToJSON action) =>
  Egg.EggConfig action state ->
  action ->
  IO ()
savePost config action =
  let bs8 = BSL.toStrict $ JSON.encode action
   in runReaderT (Egg.runEggM (MES.writeEvent bs8)) config
