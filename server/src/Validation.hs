{-# LANGUAGE DeriveGeneric #-}
module Validation where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import GHC.Generics

import App

data IsValid = NotValid [Fdb_Username] [Fdb_Password] | Valid deriving (Eq, Show, Generic)
data Fdb_Password = PasswordTooShort | BadPassword deriving (Eq, Show, Generic)
data Fdb_Username = UsernameExists | UsernameTooShort deriving (Eq, Show, Generic)

instance ToJSON IsValid
instance ToJSON Fdb_Password
instance ToJSON Fdb_Username

validatePassword :: String -> Maybe Fdb_Password
validatePassword s | length s < 10 = Just PasswordTooShort
                   | otherwise = Nothing

validateUsername :: MonadIO m => String -> AppM m [Fdb_Username]
validateUsername s = do
  mu <- getUserByName s
  let nameExists = fmap (const UsernameExists) mu
  return $ catMaybes [nameExists, tooShort]
  where tooShort = if length s >= 6 
                    then Nothing
                    else Just UsernameTooShort