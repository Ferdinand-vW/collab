{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (
    server
) where

import Control.Monad.IO.Class
import Control.Concurrent
import Data.Maybe

import Network.Wai (ResponseReceived)
import Servant
import Servant.Server.Experimental.Auth.Cookie

import App
import API
import Object.Room
import Object.User
import Template
import AppWai
import Authentication
import Validation as V

server :: Config -> Server API
server cfg = enter (appToHandle cfg) publicApi
        :<|> enter (appToHandle cfg) authApi
        :<|> servePages cfg

servePages :: Config -> Server API_Pages
servePages cfg = (Tagged . runWaiAsApp cfg . servePageRoom) 
            :<|> servePageRegister 
            :<|> servePageIndex

servePageIndex :: Server Raw
servePageIndex = serveDirectoryFileServer "static/index"

servePageRoom :: Integer -> AppHWai ResponseReceived
servePageRoom n = do
  case Just n >>= Just . liftA . App.getRoomById of
    Nothing -> do
      liftH $ throwError $ err404
    Just mroom -> mroom >>= \room -> do
      mv <- liftA getWriteLock
      liftIO $ do
        takeMVar mv
        compile "room/index.mustache" room "static/room/index.html"
        putMVar mv ()
  liftRaw $ serveDirectoryFileServer "static/room"

servePageRegister :: ServerT Raw Handler
servePageRegister = serveDirectoryFileServer "static/register"

publicApi :: ServerT API_Public (AppM Handler)
publicApi = createRoom :<|> Server.getRoomByName :<|> Server.getRoomById :<|> register

createRoom :: String -> Maybe String -> AppM Handler Integer
createRoom rname mb = maybe (makeRoom rname) (makeRoomWUser rname) mb

getRoomByName :: String -> AppM Handler (Maybe Room)
getRoomByName s = App.getRoomByName s
  
getRoomById :: Integer -> AppM Handler (Maybe Room)
getRoomById n = App.getRoomById n

register :: String -> String -> AppM Handler IsValid
register uname upass = do
  let fdb_pass = validatePassword upass
  fdb_uname <- validateUsername uname
  if null fdb_uname && null fdb_pass
    then do
      hpass <- liftIO $ hashIt upass
      case hpass of
        Nothing -> return $ NotValid [] [V.BadPassword]
        Just pass -> addUser uname pass >> return Valid
    else return $ NotValid fdb_uname (catMaybes [fdb_pass])

authApi :: ServerT API_Auth (AppM Handler)
authApi = checkLogin :<|> logout :<|> verifyLoginStatus