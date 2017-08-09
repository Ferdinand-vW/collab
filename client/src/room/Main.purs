module Room.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Int (fromString)
import Data.Maybe (Maybe (..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

main :: Eff (console :: CONSOLE) Unit
main = do
    pure unit