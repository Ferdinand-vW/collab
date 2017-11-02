{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Authentication where

import Data.ByteString.Char8
import Crypto.BCrypt
import Servant.Server
import Servant.API.BasicAuth

import Database (makeConnection, selectUserPassword)

newtype User = User { userName :: String } deriving (Eq, Show)

hashIt :: String -> IO (Maybe ByteString)
hashIt s = hashPasswordUsingPolicy fastBcryptHashingPolicy $ pack s

loginCheck :: BasicAuthCheck User
loginCheck =
  let check (BasicAuthData username password) = do
        conn <- makeConnection
        mhpw1 <- selectUserPassword conn (unpack username)
        case mhpw1 of
          Nothing -> return NoSuchUser
          Just hpw1 | validatePassword hpw1 password -> return $ Authorized (User $ unpack username)
                    | otherwise -> return BadPassword
                              
  in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = loginCheck :. EmptyContext