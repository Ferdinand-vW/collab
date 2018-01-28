{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module API where

import Data.ByteString
import Servant
import Servant.Server.Experimental.Auth.Cookie
import Authentication
import Validation

import Object.LoginForm
import Object.Room
import Object.User

api :: Proxy API
api = Proxy

type API = "api" :> "public" :> API_Public
  :<|> "api" :> "auth" :> API_Auth
  :<|> API_Pages

type API_Public = "create" :> "room" :> Capture "rname" String :> QueryParam "uname" String :> Get '[JSON] Integer
  :<|> "get" :> "room" :> "name" :> Capture "rname" String :> Get '[JSON] (Maybe Room)
  :<|> "get" :> "room" :> "id"   :> Capture "rid" Integer  :> Get '[JSON] (Maybe Room)
  :<|> "create" :> "account" :> Capture "uname" String :> Capture "upassword" String :> Get '[JSON] IsValid

type API_Auth = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Cookied String)
  :<|> "logout" :> Delete '[JSON] (Cookied ())
  :<|> "cookie" :> AuthProtect "cookie-auth" :> Post '[JSON] (Cookied String)

type API_Pages = "room" :> Capture "rid" Integer :> Raw -- get room
  :<|> "register" :> Raw -- register account
  :<|> Raw -- index
