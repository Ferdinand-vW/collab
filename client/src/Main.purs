module Main where

import Prelude
import Room.Main as R
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Browser.Location

main :: Eff (console :: CONSOLE, location :: LOCATION) Unit
main = R.main