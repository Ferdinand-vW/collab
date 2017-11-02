{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module API where

import Servant
import Authentication
import Validation

import Object.Room

type API = API_Queries
    :<|> API_Pages

api :: Proxy API
api = Proxy

type API_Queries = "api" :> (API_Public :<|> API_Private)
type API_Public = "public" :> (API_Room :<|> API_Account)
type API_Room = "create" :> "room" :> Capture "rname" String :> QueryParam "uname" String :> Get '[JSON] Integer
            :<|> "get" :> "room" :> "name" :> Capture "rname" String :> Get '[JSON] (Maybe Room)
            :<|> "get" :> "room" :> "id" :> Capture "rid" Integer :> Get '[JSON] (Maybe Room)
type API_Account = "create" :> "account" :> Capture "uname" String :> Capture "upassword" String :> Get '[JSON] IsValid
type API_Private = "private" :> API_Login
type API_Login = BasicAuth "login" User :> Get '[JSON] String

type API_Pages = Page_Room :<|> Page_Register :<|> Page_Index
type Page_Room = "room" :> Capture "rid" Integer :> Raw
type Page_Register = "register" :> Raw
type Page_Index = Raw
