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

data FormInput a = RoomId (Maybe Int) a
               | CreateRoom a

createRoomForm :: forall m. H.Component HH.HTML FormInput Void Void m
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
    ]

eval :: forall m. FormInput ~> H.ComponentDSL Int FormInput Void m
eval = case _ of
  RoomId mid next -> do
    case mid of
      Nothing -> pure next
      Just k -> do
        H.put k
        pure next
  CreateRoom next -> pure next