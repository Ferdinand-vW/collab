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
import Control.Concurrent
import Control.Monad.Reader
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Time.Clock

import Network.Wai (ResponseReceived, Request, Response)
import Servant
import Servant.Server.Internal.ServantErr (responseServantErr)
import Servant.Utils.StaticFiles

import App (AppM, appToHandle, makeRoom, makeRoomWUser, getRoomById, getRoomByName, getWriteLock, Config)
import API
import Room
import Template
import Time
import AppWai

server :: Config -> Server API
server cfg = enter (appToHandle cfg) roomApi :<|> (runWaiAsApp cfg . compileRoom) :<|> serveIndex

serveIndex :: Server HTML_Index
serveIndex = serveDirectoryFileServer "static/index"

compileRoom :: Integer -> AppMWai
compileRoom n req resp = do
  case (Just 46) >>= Just . App.getRoomById of
    Nothing -> do
      lift $ throwError $ err404
    Just mroom -> mroom >>= \room -> do
      mv <- getWriteLock
      liftIO $ do
        takeMVar mv
        compile "room/index.mustache" room "static/room/index.html"
        putMVar mv ()
  liftIO $ serveDirectoryFileServer "static/room" req resp

roomApi :: ServerT API_Room (AppM Handler)
roomApi = createRoom :<|> Server.getRoomByName :<|> Server.getRoomById

createRoom :: String -> Maybe String -> AppM Handler Integer
createRoom rname mb = maybe (makeRoom rname) (makeRoomWUser rname) mb

getRoomByName :: String -> AppM Handler (Maybe Room)
getRoomByName s = App.getRoomByName s
  
getRoomById :: Integer -> AppM Handler (Maybe Room)
getRoomById n = App.getRoomById n
