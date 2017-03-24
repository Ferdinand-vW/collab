{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (
    app, test
) where

import Servant
import Servant.HTML.Blaze
import Servant.Utils.StaticFiles

import API
import Html.Index
import Room

api' :: Proxy (RoomAPI :<|> Raw)
api' = Proxy

app :: Application
app = serve api' server

server :: Server (RoomAPI :<|> Raw)
server = room :<|> serveDirectory "client"

test :: IO Int
test = do
    x <- return 5
    y <- return "ok"
    return x

-- Server RoomAPI
room :: Int -> Handler Room
room n = return $ Room n [] []
