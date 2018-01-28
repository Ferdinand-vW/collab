{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Object.User where

import Data.Serialize
import Data.ByteString
import GHC.Generics
import Servant.Server.Experimental.Auth.Cookie (AuthCookieData)

data User = User { uid :: Integer, uname :: String, upass :: ByteString } deriving (Eq, Show, Generic)