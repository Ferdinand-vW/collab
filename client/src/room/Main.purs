module Room.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.URI
import DOM.HTML.Location
import Browser.Location

main :: Eff (console :: CONSOLE, location :: LOCATION) Unit
main = do 
    url <- getLocation
    log url
    pure unit