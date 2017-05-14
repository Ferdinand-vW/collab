module Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Trans.Class
import Data.Argonaut (decodeJson, class DecodeJson, (.?), isNull)
import Data.Argonaut.Core (Json)
import Data.DateTime
import Data.Either
import Data.HTTP.Method
import Data.Int (fromString)
import Data.JSDate (parse, toDateTime)
import Data.Maybe (Maybe (..), fromJust)
import Data.Newtype
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (location)
import DOM.HTML.Location

import DOM.HTML.Types (HISTORY)
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)

import Partial.Unsafe (unsafePartial)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM, ajax :: AJAX)) Unit
main = HA.runHalogenAff $ do
  HA.awaitLoad
  mroom <- HA.selectElement (wrap "#container")
  case mroom of
    Nothing -> liftEff $ error "Could not find anything"
    Just k -> runUI createRoomForm unit k >>= \_ -> pure unit
  
  

data FormInput a = RoomId (Maybe Int) a
               | CreateRoom a
               | JoinRoom a

createRoomForm :: forall eff. H.Component HH.HTML FormInput Unit Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
createRoomForm =
    H.component
        { initialState: const 0
        , render
        , eval
        , receiver: const Nothing
        }

render :: forall m. Int -> H.ComponentHTML FormInput
render state =
  HH.div_
    [
      HH.input
        [ HP.name "roomid"
        , HP.type_ HP.InputNumber
        , HP.required true
        , HP.placeholder "1234"
        , HE.onValueChange (HE.input (\a n -> RoomId (fromString a) n))
        ]
    , HH.button
        [ HE.onClick (HE.input_ CreateRoom)
        ]
        [ HH.text "create" ]
    , HH.button
        [
          HE.onClick (HE.input_ JoinRoom)
        ]
        [ HH.text "join room"]
    ]

eval :: forall eff. FormInput ~> H.ComponentDSL Int FormInput Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
eval = case _ of
  RoomId mid next -> do
    case mid of
      Nothing -> pure next
      Just k -> do
        H.put k
        H.liftEff $ logShow k
        pure next
  CreateRoom next -> do
    liftEff $ launchAff createRoom >>= \_ -> pure unit
    pure next
  JoinRoom next -> do
    rid <- H.get
    joinRoom rid
    pure next

createRoom = do
  res <- affjax $ defaultRequest { url = "/api/create/room/", method = Left GET }
  case decodeJson res.response :: Either String Int of
    Left x -> liftEff $ log "could not parse"
    Right rid -> liftEff $ navigateTo $ "/room/" <> show rid

joinRoom rid = liftEff $ navigateTo $ "/room/" <> show rid

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