module Index.Main where

import Index.IndexForm (indexPageForm)

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
    Just k -> runUI indexPageForm unit k >>= \_ -> pure unit