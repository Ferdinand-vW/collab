module LoginForm where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(Just, Nothing))
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)
import DOM (DOM)

data LoginFormInput a = Password_Input a
                  | UserId_Input a
                  | Login_Submit a

data LoginFormState = LoginFormState { userid :: String, pass :: String, loggedIn :: Boolean }

loginForm :: forall eff. H.Component HH.HTML LoginFormInput Unit Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM | eff))
loginForm =
    H.component
        { initialState: const $ LoginFormState { userid: "", pass: "", loggedIn: false }
        , render
        , eval
        , receiver: const Nothing
        }

render :: LoginFormState -> H.ComponentHTML LoginFormInput
render (LoginFormState s) | s.loggedIn = renderLogout
         | otherwise = renderLogin

renderLogout :: H.ComponentHTML LoginFormInput
renderLogout = HH.div 
  [ HP.class_ $ H.ClassName "navbar-right"]
  [ ]


<div id="navbar" class="navbar-right" style="height:100%;padding:0px;margin:0px;">
          <ul class="nav navbar-nav" style="margin:0px;padding:0px;">
            <li><a href="#">Logged in as Guest #123</a></li>
            <li><a href="#about">Logout</a></li>
            <li><a href="#contact">Settings</a></li>
          </ul>
        </div>

renderLogin :: H.ComponentHTML LoginFormInput
renderLogin = HH.div [] []

eval :: forall eff. LoginInput ~> H.ComponentDSL LoginFormState LoginFormInput Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM | eff))
eval li = case li of
  Password_Input next -> do
    pure next
  Login_Input next -> do
    pure next
  Login_Submit next -> do
    pure next