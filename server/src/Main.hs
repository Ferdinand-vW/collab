{-# LANGUAGE TypeOperators #-}
module Main where

import Servant (serve, enter, Server, ServerT, Handler, (:~>))
import App
import Network.Wai.Handler.Warp (run)
import Control.Monad.Reader
import Server (server)
import API (api, API, API_All)

main :: IO ()
main = do
  config <- newConfig
  run 8082 $ serve api $ server config
