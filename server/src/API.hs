{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module API where

import Data.Aeson.Compat
import GHC.Generics

import Servant
import Servant.HTML.Blaze

import Room

type API = API_All
    :<|> HTML_Room
    :<|> Raw

type HTML_Room = "room" :> Capture "rid" Integer :> Raw
type HTML_Index = Raw
type API_All = "api" :> API_Room
type API_Room = "create" :> "room" :> Capture "rname" String :> QueryParam "uname" String :> Get '[JSON] Integer
            :<|> "get" :> "room" :> "name" :> Capture "rname" String :> Get '[JSON] (Maybe Room)
            :<|> "get" :> "room" :> "id" :> Capture "rid" Integer :> Get '[JSON] (Maybe Room)

api :: Proxy API
api = Proxy
