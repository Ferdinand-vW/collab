{-# LANGUAGE TypeOperators #-}
module Main where

import Servant (serveWithContext)
import Network.Wai.Handler.Warp (run)
import App
import Server (server)
import API (api)
import Authentication

main :: IO ()
main = do
  config <- newConfig
  run 8082 $ serveWithContext api basicAuthServerContext $ server config
