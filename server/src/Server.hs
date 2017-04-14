{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (
    app
) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Servant
import Servant.Utils.StaticFiles
import Servant.HTML.Blaze
import Text.Mustache

import API
import Room

app :: Application
app = serve api server

server :: Server API
server = room :<|> serveRoom :<|> serveIndex :<|> serveIndex

serveIndex :: Server HTML_Index
serveIndex = serveDirectoryFileServer "static/index"

serveRoom :: Server HTML_Room
serveRoom n req resp = do
    compiled <- liftIO $ automaticCompile ["./templates"] "index.mustache"
    x <- case compiled of
            Left err -> liftIO (print err)
            Right template -> do
                let t = substitute template (Room 1 "test")
                liftIO $ TIO.writeFile "static/room/index.html" t 

    x `seq` serveDirectoryFileServer "static/room" req resp

-- API_Room
room :: Int -> Handler Room
room n = return $ Room n "Test"

instance ToMustache Room where
    toMustache room = object
        [ T.pack "id" ~> Room.id room]