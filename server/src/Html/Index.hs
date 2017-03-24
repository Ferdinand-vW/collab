{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Html.Index where

import Data.List
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Room


indexPage :: Html
indexPage = docTypeHtml $ do
    H.head $ H.title "My title"
    body $ H.div roomForm


roomForm :: Html
roomForm = docTypeHtml $ do
    H.form ! method "GET" ! action "/room" $ do
        input ! name "id" ! type_ "text" ! placeholder "1234" ! required "True"
        input ! type_ "submit" ! value "join room"


roomPage :: Room -> Html
roomPage (Room n users code) = docTypeHtml $ do
    H.head $ H.title "A room"
    body $ do
        p $ "this is room " >> toHtml n
        p $ "The following users: " >> toHtml (intercalate "," users)
        p $ toHtml $ concatMap (++ "\n") code

errorPage :: Html
errorPage = docTypeHtml $ do
    H.head $ H.title "Error"
    body $ "Invalid room number."