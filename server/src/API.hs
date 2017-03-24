{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module API where

import Data.Aeson.Compat
import GHC.Generics
import Servant

import Text.Blaze.Html5
import Servant.HTML.Blaze

import Room
import Html.Index

data Page = IndexPage | RoomPage Room | ErrorPage

type API = IndexAPI :<|> RoomAPI
type IndexAPI = Get '[HTML] Page

instance ToMarkup Page where
    toMarkup IndexPage = indexPage
    toMarkup (RoomPage r) = roomPage r
    toMarkup ErrorPage = errorPage

api :: Proxy API
api = Proxy

indexAPI :: Proxy IndexAPI
indexAPI = Proxy

type RoomAPI = "room" :> Capture "id" Int :> Get '[JSON] Room

roomAPI :: Proxy RoomAPI
roomAPI = Proxy