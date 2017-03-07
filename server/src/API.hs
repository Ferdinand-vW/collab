{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module API where

import Data.Aeson.Compat
import GHC.Generics
import Servant

import Text.Blaze.Html5
import Servant.HTML.Blaze
import Html.Index

data Page = IndexPage

type API = IndexAPI :<|> RoomAPI
type IndexAPI = Get '[HTML] Page

instance ToMarkup Page where
    toMarkup IndexPage = indexPage

api :: Proxy API
api = Proxy

indexAPI :: Proxy IndexAPI
indexAPI = Proxy

type RoomAPI = "room" :> QueryParam "id" Int :> Get '[JSON] Room

roomAPI :: Proxy RoomAPI
roomAPI = Proxy

data Room = Room
    { roomName :: String
    , users :: [String]
    , messages :: [String]
    , code :: [String]
    } deriving Generic

instance ToJSON Room