module Main where

import Component.IndexForm (insertIndexForm)
import Component.LoginForm (insertLoginForm)
import Component.RegistrationForm (insertRegistrationForm)

import Prelude
import Halogen.Aff as HA
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

main :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM, ajax :: AJAX)) Unit
main = do
  insertIndexForm
  insertLoginForm
  insertRegistrationForm
  