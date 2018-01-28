{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Authentication where

import Data.ByteString.Char8
import Data.Serialize
import Crypto.BCrypt
import Control.Monad.Catch (try)
import Control.Monad.Trans.Class
import GHC.Generics
import GHC.TypeLits (Symbol)
import Network.Wai (Request)
import Servant
import Servant.Server
import Servant.API.BasicAuth
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server.Experimental.Auth.Cookie

import Object.LoginForm
import Object.User
import qualified Object.Account as A
import App
import Database (makeConnection, selectUserPassword)

hashIt :: String -> IO (Maybe ByteString)
hashIt s = hashPasswordUsingPolicy fastBcryptHashingPolicy $ pack s


checkLogin :: LoginForm -> AppM Handler (Cookied String)
checkLogin loginForm = do
  mu <- getUserByName $ username loginForm
  case mu of
    Nothing -> lift $ throwError err401
    Just user | validatePassword (upass user) (pack $ password loginForm) -> addSession' (A.Account (uid user) (uname user)) (uname user)
              | otherwise -> lift $ throwError err401

verifyLoginStatus :: WithMetadata A.Account -> AppM Handler (Cookied String)
verifyLoginStatus wmuser = cookied' A.username wmuser

logout :: AppM Handler (Cookied ())
logout = removeSession' ()

basicAuthServerContext :: AuthCookieSettings -> PersistentServerKey -> Context (AuthHandler Request (WithMetadata A.Account) ': '[])
basicAuthServerContext settings sk = sessionAuthHandler settings sk :. EmptyContext

sessionAuthHandler :: AuthCookieSettings -> PersistentServerKey -> AuthHandler Request (WithMetadata A.Account)
sessionAuthHandler settings key = do
  mkAuthHandler $ \req -> do
    result <- try (getSession settings key req)
    case result :: Either AuthCookieException (Maybe (WithMetadata A.Account)) of
      Right (Just acc) -> return acc
      _ -> throwError err401

addSession' :: AddHeader (e :: Symbol) EncryptedSession s r => A.Account -> s -> AppM Handler r
addSession' u s = do
  config <- getConfig
  lift $ addSession (cookies config) (rndSource config) (serverKey config) u s

removeSession' :: AddHeader (e :: Symbol) EncryptedSession s r => s -> AppM Handler r
removeSession' s = do
  config <- getConfig
  lift $ removeSession (cookies config) s

cookied' :: (A.Account -> String) -> WithMetadata A.Account -> AppM Handler (Cookied String)
cookied' f mdu = do
  config <- getConfig
  lift $ cookied (cookies config) (rndSource config) (serverKey config) f mdu