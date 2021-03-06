{-# LANGUAGE DeriveGeneric #-}

module Object.Room where

import Data.Aeson.Compat (ToJSON)
import GHC.Generics
import Text.Mustache
import qualified Data.Text                  as T
import Data.Time.Clock

import Time

data Room = Room
    { rid :: Integer
    , rname :: String
    , rcreated :: DateTime
    , r_uid :: Maybe Integer
    } deriving Generic

instance ToJSON Room

instance ToMustache Room where
    toMustache room = object
        [ T.pack "id" ~> rid room]

data RoomNotFound = RoomNotFound Integer

instance ToMustache RoomNotFound where
  toMustache (RoomNotFound n) = object
        [ T.pack "rid" ~> n]