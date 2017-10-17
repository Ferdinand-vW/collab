{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (
    server,
    roomApi
) where

import Control.Monad.IO.Class
import Control.Concurrent

import Network.Wai (ResponseReceived)
import Servant

import App (AppM, appToHandle, makeRoom, makeRoomWUser, getRoomById, getRoomByName, getWriteLock, Config)
import API
import Room
import Template
import AppWai

server :: Config -> Server API
server cfg = enter (appToHandle cfg) roomApi :<|> (Tagged . runWaiAsApp cfg . compileRoom) :<|> serveIndex

serveIndex :: Server HTML_Index
serveIndex = serveDirectoryFileServer "static/index"

compileRoom :: Integer -> AppHWai ResponseReceived
compileRoom n = do
  case (Just 46) >>= Just . liftA . App.getRoomById of
    Nothing -> do
      liftH $ throwError $ err404
    Just mroom -> mroom >>= \room -> do
      mv <- liftA getWriteLock
      liftIO $ do
        takeMVar mv
        compile "room/index.mustache" room "static/room/index.html"
        putMVar mv ()
  liftRaw $ serveDirectoryFileServer "static/room"

roomApi :: ServerT API_Room (AppM Handler)
roomApi = createRoom :<|> Server.getRoomByName :<|> Server.getRoomById

createRoom :: String -> Maybe String -> AppM Handler Integer
createRoom rname mb = maybe (makeRoom rname) (makeRoomWUser rname) mb

getRoomByName :: String -> AppM Handler (Maybe Room)
getRoomByName s = App.getRoomByName s
  
getRoomById :: Integer -> AppM Handler (Maybe Room)
getRoomById n = App.getRoomById n
