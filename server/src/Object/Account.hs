{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Object.Account where

import Data.Serialize
import GHC.Generics
import Servant.Server.Experimental.Auth.Cookie

data Account = Account { uid :: Integer, username :: String } deriving Generic

instance Serialize Account
type instance AuthCookieData = Account