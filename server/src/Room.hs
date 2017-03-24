{-# LANGUAGE DeriveGeneric #-}

module Room where

import Data.Aeson.Compat
import GHC.Generics

data Room = Room
    { roomName :: Int
    , users :: [String]
    , code :: [String]
    } deriving Generic

instance ToJSON Room