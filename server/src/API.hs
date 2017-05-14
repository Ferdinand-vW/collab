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
    :<|> HTML_Index

type HTML_Room = "room" :> Capture "id" Integer :> Raw
type HTML_Index = Raw
type API_All = "api" :> API_Room
type API_Room = "create" :> "room" :> QueryParam "user" String :> Get '[JSON] Integer
            :<|>
                "room" :> Capture "id" Integer :> Get '[JSON] (Maybe Room)

api :: Proxy API
api = Proxy
