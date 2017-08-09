module Main where

import Index.Main as I

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Halogen.Aff as HA
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

main :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM, ajax :: AJAX)) Unit
main = I.main
  
  



