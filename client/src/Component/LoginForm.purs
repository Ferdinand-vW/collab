module Component.LoginForm where

import Prelude (type (~>), Unit, Void, bind, const, discard, otherwise, pure, show, unit, ($), (/=), (<>), (>>=))
import Data.Either (Either(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Halogen (liftAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax, defaultRequest)
import Text.Base64 (encode64)

insertLoginForm :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM, ajax :: AJAX)) Unit
insertLoginForm = HA.runHalogenAff $ do
  HA.awaitLoad
  lform <- HA.selectElement $ wrap "#login-form"
  case lform of
    Nothing -> liftEff $ log "Could not find #login-form"
    Just k -> runUI loginForm unit k >>= \_ -> pure unit

data LoginFormInput a = Password_Input String a
                  | UserId_Input String a
                  | Login_Submit a

data LoginFormState = LoginFormState { userid :: String, pass :: String, loggedIn :: Boolean }

loginForm :: forall eff. H.Component HH.HTML LoginFormInput Unit Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
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
renderLogout = HH.li_ 
  [
    HH.a_ [ HH.text "Logout" ]
  ]

renderLogin :: H.ComponentHTML LoginFormInput
renderLogin = HH.form [ HP.class_ $ H.ClassName "navbar-form navbar-right" ]
  [ HH.div [ HP.class_ $ H.ClassName "form-group"]
    [ HH.input
      [ HP.type_ HP.InputText
      , HP.class_ $ H.ClassName "form-control"
      , HP.placeholder "UserId"
      , HE.onValueInput $ HE.input UserId_Input 
      ]
    , HH.input
      [ HP.type_ HP.InputPassword
      , HP.class_ $ H.ClassName "form-control"
      , HP.placeholder "Password"
      , HE.onValueInput $ HE.input Password_Input 
      ]
    ]
  , HH.button
    [ HP.type_ HP.ButtonSubmit
    , HP.class_ $ H.ClassName "btn btn-default"
    , HE.onClick $ const $ Just $ Login_Submit unit
    ]
    [ HH.text "Login" ]
  ]

eval :: forall eff. LoginFormInput ~> H.ComponentDSL LoginFormState LoginFormInput Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
eval li = case li of
  Password_Input s next -> do
    LoginFormState lfs <- H.get
    H.put $ LoginFormState $ lfs { pass = s }
    pure next
  UserId_Input s next -> do
    LoginFormState lfs <- H.get
    H.put $ LoginFormState $ lfs { userid = s }
    pure next
  Login_Submit next -> do
    LoginFormState lfs <- H.get
    H.put $ LoginFormState { userid: "", pass: "", loggedIn: true }
    _ <- liftAff $ login lfs.userid lfs.pass
    pure next

login :: forall eff. String -> String -> Aff (console :: CONSOLE, ajax :: AJAX| eff ) Unit
login uname pw = do
  liftEff $ log uname
  res :: AffjaxResponse String <- affjax $ defaultRequest { url = "/api/private/", headers = [RequestHeader "Authorization" ("Basic " <> encode64 (uname <> ":" <> pw))], method = Left GET }
  if res.status /= (StatusCode 200)
    then liftEff $ log $ show res.status
    else liftEff $ log "success"
