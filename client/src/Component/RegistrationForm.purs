module Component.RegistrationForm where

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Component.IndexForm (navigateTo)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (null, take)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Data.Traversable (sequence)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Prelude (type (~>), Unit, Void, bind, const, discard, map, pure, unit, ($), (<>), (>>=))


data RFS = RFS { userid :: String, pass :: String, fdb_Username :: Array Fdb_Username, fdb_Password :: Array Fdb_Password }
data RFA a = EnterUserId String a | EnterPassword String a | Register a

insertRegistrationForm :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM, ajax :: AJAX)) Unit
insertRegistrationForm = HA.runHalogenAff $ do
  HA.awaitLoad
  lform <- HA.selectElement $ wrap "#registration-form"
  case lform of
    Nothing -> liftEff $ log "Could not find #registration-form"
    Just k -> runUI registrationForm unit k >>= \_ -> pure unit

registrationForm :: forall eff. H.Component HH.HTML RFA Unit Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
registrationForm =
    H.component
        { initialState: const $ RFS { userid: "", pass: "", fdb_Username: [], fdb_Password: [] }
        , render
        , eval
        , receiver: const Nothing
        }

render :: RFS -> H.ComponentHTML RFA
render rfs = HH.form_
  [ renderInputUsername rfs
  , renderInputPassword rfs
  , HH.input
    [ HP.type_ HP.InputButton
    , HP.class_ $ H.ClassName "btn btn-default"
    , HE.onClick $ const $ Just $ Register unit
    , HP.value "Register"
    ]
  ]

renderInputUsername :: RFS -> H.ComponentHTML RFA
renderInputUsername (RFS rfs) = renderInput "Username:" HP.InputText EnterUserId (map renderFdb_Username rfs.fdb_Username)

renderInputPassword :: RFS -> H.ComponentHTML RFA
renderInputPassword (RFS rfs) = renderInput "Password:" HP.InputPassword EnterPassword (map renderFdb_Password rfs.fdb_Password)

renderInput :: String -> HP.InputType -> (String -> Unit -> RFA Unit) -> Array (H.ComponentHTML RFA) -> H.ComponentHTML RFA
renderInput s inputType action feedback = 
  HH.div 
    [ HP.class_ $ H.ClassName $ "form-group" <> addHasError ]
    ([ HH.label_
      [ HH.text s]
    , HH.input 
      [ HP.type_ inputType
      , HP.class_ $ H.ClassName "form-control"
      , HE.onValueInput $ HE.input action
      ]
    ] <> feedback)
  where 
    addHasError | null feedback = ""
                | otherwise = " has-error"

renderFdb_Username :: Fdb_Username -> H.ComponentHTML RFA
renderFdb_Username fdb = 
  HH.span 
  [ HP.class_ $ H.ClassName "help-block" ]
  [ HH.text $ fdb_UsernameToText fdb ]

renderFdb_Password :: Fdb_Password -> H.ComponentHTML RFA
renderFdb_Password fdb = 
  HH.span 
  [ HP.class_ $ H.ClassName "help-block" ]
  [ HH.text $ fdb_PasswordToText fdb ]

fdb_UsernameToText :: Fdb_Username -> String
fdb_UsernameToText UsernameTooShort = "Username length must be at least 6 characters"
fdb_UsernameToText UsernameExists = "This username already exists. Please choose a different one"

fdb_PasswordToText :: Fdb_Password -> String
fdb_PasswordToText PasswordTooShort = "Password length must be at least 10 characters"
fdb_PasswordToText BadPassword = "Password does not satisfy all requirements" 

eval :: forall eff. RFA ~> H.ComponentDSL RFS RFA Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
eval (EnterUserId s next) = do
  RFS rfs <- H.get
  H.put $ RFS $ rfs { userid = s }
  pure next
eval (EnterPassword s next) = do
  RFS rfs <- H.get
  H.put $ RFS $ rfs { pass = s }
  pure next
eval (Register next) = do
  RFS rfs <- H.get
  val <- H.liftAff $ createAccount (RFS rfs)
  case val of
    Valid -> do
      H.liftEff $ navigateTo "/"
      H.liftEff $ log "account created"
    NotValid fdb_u fdb_p -> do 
      H.put $ RFS { userid: rfs.userid, pass: rfs.pass, fdb_Username: fdb_u, fdb_Password: fdb_p }
      H.liftEff $ log "invalid account details"
  pure next

createAccount :: forall eff. RFS -> Aff (ajax :: AJAX, console :: CONSOLE | eff) IsValid
createAccount (RFS rfs) = do
  liftEff $ log rfs.pass
  res <- affjax $ defaultRequest { url = "/api/public/create/account/" <> rfs.userid <> "/" <> rfs.pass <> "/", method = Left GET }
  case decodeJson res.response :: Either String IsValid of
    Left err -> do
      liftEff $ log err
      pure $ NotValid [] []
    Right val -> pure val

data IsValid = NotValid (Array Fdb_Username) (Array Fdb_Password) | Valid
data Fdb_Password = PasswordTooShort | BadPassword
data Fdb_Username = UsernameExists | UsernameTooShort

instance decodeJsonIsValid :: DecodeJson IsValid where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Valid" -> pure Valid
      _ -> do
        conts <- obj .? "contents"
        case take 2 conts of
          [cont_fdb_u, cont_fdb_p] -> do
            fdb_u <- sequence $ map toFdb_Username cont_fdb_u
            fdb_p <- sequence $ map toFdb_Password cont_fdb_p
            pure $ NotValid fdb_u fdb_p
          _ -> Left "Could not parse contents"

toFdb_Username :: String -> Either String Fdb_Username
toFdb_Username s = case s of
  "UsernameTooShort" -> Right $ UsernameTooShort
  "UsernameExists" -> Right $ UsernameExists
  _ -> Left "Could not parse Fdb_Username"

toFdb_Password :: String -> Either String Fdb_Password
toFdb_Password s = case s of
  "PasswordTooShort" -> Right $ PasswordTooShort
  "BadPassword" -> Right $ BadPassword
  _ -> Left "Could not parse Fdb_Password"

