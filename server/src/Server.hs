{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (
    app
) where

import Servant
import Servant.HTML.Blaze

import API
import Html.Index

app :: Application
app = serve indexAPI server

server :: Server IndexAPI
server = return IndexPage

