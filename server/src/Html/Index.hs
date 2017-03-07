{-# LANGUAGE OverloadedStrings #-}
module Html.Index where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5 as A


indexPage :: A.Html
indexPage = H.docTypeHtml $ do
    H.head $ H.title "My title"
    H.body $ H.p "this is a paragraph"