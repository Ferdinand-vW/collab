module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

main = Eff (console :: Console) Unit
main = pure unit