module Component.IndexForm where

import Prelude (class Show, type (~>), Unit, Void, bind, bottom, const, discard, pure, show, unit, ($), (<>), (>>=))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (replace)
import DOM.HTML.Window (location)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.DateTime (Date, DateTime(DateTime), Time, canonicalDate, setHour, setMillisecond, setMinute, setSecond)
import Data.Enum (toEnum)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.JSDate (parse, toDateTime)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)

data FormInput a = JoinRoom a
               | JoinRoomInput String a
               | CreateRoom a
               | CreateRoomInput String a 

insertIndexForm :: Eff (HA.HalogenEffects (console :: CONSOLE, dom :: DOM, ajax :: AJAX)) Unit
insertIndexForm = HA.runHalogenAff $ do
  HA.awaitLoad
  mroom <- HA.selectElement (wrap "#index-form")
  case mroom of
    Nothing -> liftEff $ log "Could not find #index-form"
    Just k -> runUI indexPageForm unit k >>= \_ -> pure unit

indexPageForm :: forall eff. H.Component HH.HTML FormInput Unit Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
indexPageForm =
    H.component
        { initialState: const ""
        , render
        , eval
        , receiver: const Nothing
        }

render :: String -> H.ComponentHTML FormInput
render state =
  HH.div
    [ HP.class_ $ H.ClassName "form-group" ]
    [ HH.div
      [ HP.class_ $ H.ClassName "input-group"]
      [ HH.input
        [ HP.type_ HP.InputText
        , HP.class_ $ H.ClassName "form-control"
        , HP.placeholder "Enter a room number"
        , HE.onValueInput $ HE.input JoinRoomInput]
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
        , HP.placeholder "Enter a room number"
        , HE.onValueInput $ HE.input CreateRoomInput]
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

eval :: forall eff. FormInput ~> H.ComponentDSL String FormInput Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM, exception :: EXCEPTION | eff))
eval fi = case fi of
  CreateRoom next -> do
    name <- H.get
    liftEff $ launchAff (createRoom name) >>= \_ -> pure unit
    pure next
  CreateRoomInput name next -> do
    H.put name
    pure next
  JoinRoom next -> do
    name <- H.get
    _ <- liftEff $ launchAff (joinRoom name)
    pure next
  JoinRoomInput name next -> do
    H.put name
    pure next

createRoom :: forall aff. String -> Aff ( ajax :: AJAX , console :: CONSOLE, dom :: DOM | aff) Unit              
createRoom name = do
  res <- affjax $ defaultRequest { url = "/api/create/room/" <> name, method = Left GET }
  case decodeJson res.response :: Either String Int of
    Left x -> liftEff $ log "could not parse"
    Right rid -> liftEff $ navigateTo $ "/room/" <> show rid <> "/"

joinRoom :: forall aff. String -> Aff (ajax :: AJAX, console :: CONSOLE, dom :: DOM | aff) Unit
joinRoom name = do
  res <- affjax $ defaultRequest { url = "api/get/room/name/" <> name, method = Left GET }
  liftEff $ log $ show res.response
  case decodeJson res.response :: Either String Room of
    Left x -> liftEff $ log x
    Right (Room rm) -> do
      liftEff $ navigateTo $ "/room/" <> show rm.rid <> "/"

navigateTo :: forall eff. String -> Eff (dom :: DOM | eff) Unit     
navigateTo path = do
  w <- window
  loc <- location w
  replace path loc

data Room = Room 
        { rid :: Int
        , rname :: String
        , rcreated :: DateTime
        , uid :: Maybe Int
        }

instance decodeJsonRoom :: DecodeJson Room where
  decodeJson json = do
    obj <- decodeJson json
    rid <- obj .? "rid"
    rname <- obj .? "rname"
    uid <- obj .? "uid"
    (DateTimeNT rcreated) <- obj .? "rcreated"
    pure $ Room { rid, rname, rcreated, uid }


data DateTimeNT = DateTimeNT DateTime
data DateNT = DateNT Date
data TimeNT = TimeNT Time

instance decodeJsonDateTimeNT :: DecodeJson DateTimeNT where
  decodeJson json = do
    obj <- decodeJson json
    DateNT date <- obj .? "date"
    TimeNT time <- obj .? "time"
    
    pure $ DateTimeNT $ DateTime date time

instance decodeJsonDateNT :: DecodeJson DateNT where
  decodeJson json = do
    obj <- decodeJson json
    year <- obj .? "year"
    month <- obj .? "month"
    day <- obj .? "day"
    let mdate = do
                  year' <- toEnum year
                  month' <- toEnum month
                  day' <- toEnum day
                  pure $ DateNT $ canonicalDate year' month' day'
    case mdate of
      Nothing -> Left "Could not parse date"
      Just date -> Right date

instance decodeJsonTimeNT :: DecodeJson TimeNT where
  decodeJson json = do
    obj <- decodeJson json
    h <- obj .? "h"
    m <- obj .? "m"
    s <- obj .? "s"
    ml <- obj .? "ml"
    let mtime = do
                h' <- toEnum h
                m' <- toEnum m
                s' <- toEnum s
                ml' <- toEnum ml
                pure $ TimeNT $ setHour h' $ setMinute m' $ setSecond s' $ setMillisecond ml' bottom
    case mtime of
      Nothing -> Left "Failed to parse Time"
      Just tm -> Right tm



-- unsafe stuff happening here, since there is no other way to
-- parse a date time within a json decoder
unsafeToDateTime :: String -> Either String DateTime
unsafeToDateTime s = unsafePerformEff $ do
  jsdate <- parse s
  pure $ case toDateTime jsdate of
    Nothing -> Left "Could not parse DateTime2"
    Just dt -> Right dt

instance showRoom :: Show Room where
  show (Room rm) = "Roomy " <> rm.rname <> show rm.rid <> " " <> show rm.uid 