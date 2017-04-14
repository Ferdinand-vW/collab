{-# LANGUAGE DeriveGeneric #-}

module Room where

import Data.Aeson.Compat
import GHC.Generics

data Room = Room
    { id :: Int
    , name :: String
    } deriving Generic

instance ToJSON Room