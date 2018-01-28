{-# LANGUAGE DeriveGeneric #-}
module Object.LoginForm where

import Data.Aeson.Compat (ToJSON, FromJSON)
import GHC.Generics (Generic)

data LoginForm = LoginForm 
  { username :: String
  , password :: String
  } deriving Generic

instance ToJSON LoginForm
instance FromJSON LoginForm