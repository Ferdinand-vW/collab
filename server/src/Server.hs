{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (
    server,
    roomApi
) where

import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Time.Clock

import Network.Wai (ResponseReceived, Request, Response)
import Servant
import Servant.Server.Internal.ServantErr (responseServantErr)
import Servant.Utils.StaticFiles

import App (AppM, appToHandle, makeRoom, makeRoomWUser, getRoom)
import Database (insertRoom, selectRoom)
import API
import Room
import Template
import Time
import AppWai

server :: Server API
server = enter appToHandle roomApi :<|> (runWaiAsApp . serveRoom) :<|> serveIndex

serveIndex :: Server HTML_Index
serveIndex = serveDirectoryFileServer "static/index"

serveRoom :: Integer -> AppMWai
serveRoom n req resp = do
  mroom <- App.getRoom n
  case mroom of
    Nothing -> do
      lift $ throwError $ err404
    Just room -> liftIO $ compile "room/index.mustache" room "static/room/index.html"

  liftIO $ serveDirectoryFileServer "static/room" req resp

roomApi :: ServerT API_Room (AppM Handler)
roomApi = createRoom :<|> Server.getRoom

createRoom :: Maybe String -> AppM Handler Integer
createRoom Nothing = makeRoom
createRoom (Just uname) = makeRoomWUser uname

  

-- API_Room
getRoom :: Integer -> AppM Handler (Maybe Room)
getRoom n = App.getRoom n
