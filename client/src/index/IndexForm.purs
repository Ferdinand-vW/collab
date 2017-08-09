module Index.IndexForm where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.JSDate (parse, toDateTime)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location)
import DOM.HTML.Location (replace)
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)
import Partial.Unsafe (unsafePartial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data FormInput a = CreateRoom a
               | JoinRoom a

indexPageForm :: forall eff. H.Component HH.HTML FormInput Unit Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
indexPageForm =
    H.component
        { initialState: const 0
        , render
        , eval
        , receiver: const Nothing
        }

render :: Int -> H.ComponentHTML FormInput
render state =
  HH.div
    [ HP.class_ $ H.ClassName "form-group" ]
    [ HH.div
      [ HP.class_ $ H.ClassName "input-group"]
      [ HH.input
        [ HP.type_ HP.InputText
        , HP.class_ $ H.ClassName "form-control"
        , HP.placeholder "Enter a room number"]
      ,
        HH.span
        [ HP.class_ $ H.ClassName "input-group-btn"]
        [ HH.button
          [ HP.class_ $ H.ClassName "btn btn-default btn-block"
          , HP.type_ HP.ButtonSubmit
          , HE.onClick $ const $ Just $ JoinRoom unit]
          [ HH.text "Enter room" ]
        ]
      ]

    , HH.div
      [ HP.class_ $ H.ClassName "input-group"]
      [ HH.input
        [ HP.type_ HP.InputText
        , HP.class_ $ H.ClassName "form-control"
        , HP.placeholder "Enter a room number"]
      ,
        HH.span
        [ HP.class_ $ H.ClassName "input-group-btn"]
        [ HH.button
          [ HP.class_ $ H.ClassName "btn btn-default btn-block"
          , HP.type_ HP.ButtonSubmit
          , HE.onClick $ const $ Just $ CreateRoom unit]
          [ HH.text "Create room" ]
        ]
      ]
    ]

eval :: forall eff. FormInput ~> H.ComponentDSL Int FormInput Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
eval fi = case fi of
  CreateRoom next -> do
    liftEff $ launchAff createRoom >>= \_ -> pure unit
    pure next
  JoinRoom next -> do
    rid <- H.get
    joinRoom rid
    pure next

createRoom :: forall t205.            
  Aff                   
    ( ajax :: AJAX      
    , console :: CONSOLE
    , dom :: DOM        
    | t205              
    )                   
    Unit
createRoom = do
  res <- affjax $ defaultRequest { url = "/api/create/room/", method = Left GET }
  case decodeJson res.response :: Either String Int of
    Left x -> liftEff $ log "could not parse"
    Right rid -> liftEff $ navigateTo $ "/room/" <> show rid

joinRoom :: forall t140 t144 t146.              
  MonadEff                          
    ( dom :: DOM                    
    | t144                          
    )                               
    t140                            
   => Show t146 => t146 -> t140 Unit
joinRoom rid = liftEff $ navigateTo $ "/room/" <> show rid

navigateTo :: forall t129.       
  String           
  -> Eff           
       ( dom :: DOM
       | t129      
       )           
       Unit
navigateTo path = do
  w <- window
  loc <- location w
  replace path loc

data Room = Room 
        { rid :: Int
        , cr :: DateTime
        , uid :: Int
        }

instance decodeJsonRoom :: DecodeJson Room where
  decodeJson json = do
    obj <- decodeJson json
    rid <- obj .? "rid"
    cr' <- obj .? "created"
    uid <- obj .? "name"
    let cr = unsafeToDateTime cr'
    pure $ Room { rid, cr, uid }


-- unsafe stuff happening here, since there is no other way to
-- parse a date time within a json decoder
unsafeToDateTime :: String -> DateTime
unsafeToDateTime s = unsafePerformEff $ do
  jsdate <- parse s
  unsafePartial $ case toDateTime jsdate of
    Just dt -> pure dt

instance showRoom :: Show Room where
  show (Room {rid, cr, uid}) = "Roomy " <> show rid <> " " <> show uid 