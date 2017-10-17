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


import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

data LoginFormInput a = Password_Input a
                  | UserId_Input a
                  | Login_Submit a

data LoginFormState = LoginFormState { userid :: String, pass :: String, loggedIn :: Boolean }
{- 
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
  [ ] -}


{- <div id="navbar" class="navbar-right" style="height:100%;padding:0px;margin:0px;">
          <ul class="nav navbar-nav" style="margin:0px;padding:0px;">
            <li><a href="#">Logged in as Guest #123</a></li>
            <li><a href="#about">Logout</a></li>
            <li><a href="#contact">Settings</a></li>
          </ul>
        </div> -}

{- renderLogin :: H.ComponentHTML LoginFormInput
renderLogin = HH.div [] []

eval :: forall eff. LoginInput ~> H.ComponentDSL LoginFormState LoginFormInput Void (Aff (console :: CONSOLE, ajax :: AJAX, dom :: DOM | eff))
eval li = case li of
  Password_Input next -> do
    pure next
  Login_Input next -> do
    pure next
  Login_Submit next -> do
    pure next
 -}








type Entry =
    { firstName :: String
    , lastName  :: String
    , address   :: Address
    }

type Address =
    { street :: String
    , city   :: String
    , state  :: String
    }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons


findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry first last = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == first && entry.lastName == last

printEntry :: String -> String -> AddressBook -> Maybe String
--printEntry first last book = map showEntry $ findEntry first last book
printEntry = ((<<<) ((<<<) (map showEntry))) <<< findEntry
{-

Could not match type
                   
    Function String
                   
  with type
          
    Record
          

while trying to match type String                                 
                           -> List                                
                                { firstName :: String             
                                , lastName :: String              
                                , address :: { street :: String   
                                             , city :: String     
                                             , state :: String    
                                             }                    
                                }                                 
                              -> Maybe                            
                                   { firstName :: String          
                                   , lastName :: String           
                                   , address :: { street :: String
                                                , city :: String  
                                                , state :: String 
                                                }                 
                                   }                              
  with type { firstName :: String          
            , lastName :: String           
            , address :: { street :: String
                         , city :: String  
                         , state :: String 
                         }                 
            }                              
while checking that expression findEntry
  has type t0                               
             { firstName :: String          
             , lastName :: String           
             , address :: { street :: String
                          , city :: String  
                          , state :: String 
                          }                 
             }                              
in value declaration printEntry

where t0 is an unknown type

-}